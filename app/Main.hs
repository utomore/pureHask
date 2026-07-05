-- | The effectful shell: SDL window, the frame loop, and execution of
--   'FlowCommand's. This module wires the layers together but decides
--   nothing itself:
--
--     * raw SDL events   -> 'RawInput'                (translation only)
--     * 'RawInput'       -> "FRP.Network" 'netFrame'  (intents, mode, commands)
--     * intents          -> fixed-step simulation     (only in 'ModePlaying')
--     * simulation facts -> "FRP.Network" 'netEvents' (mode reacts)
--     * ECS world        -> "Render.Draw" / "Render.UI" / "Render.Menu"
module Main where

import Control.Monad (unless, when, foldM)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word (Word32)
import Apecs (runSystem, cfold)
import qualified SDL
import Linear (V2(..), V4(..))

import Core.Components (World, initWorld, Player(..), Backpack(..), Equipped(..),
                        Position(..), Collider(..))
import Core.Config
import Core.Settings
import Core.Types
import FRP.Network
import Items.Registry (ItemRegistry, loadRegistry, requireItem)
import Npc.Script (NpcDef(..), loadNpcDir, npcItemRefs, npcQuestRefs)
import Quest.Runtime (activeGoal)
import Quest.Script (QuestDef(..), loadQuestDir, questItemRefs)
import Render.Draw (renderScene)
import Render.Font (drawText)
import Render.Hud (renderHud)
import Render.Menu (renderMenu)
import Render.UI (renderOverlay, renderBackpack)
import Sim.Combat (controlPlayer)
import Sim.Items (backpackRows)
import Sim.Npc (spawnNpcsForLevel, stepNpcs, runNpcCommand, interactNearest)
import Sim.Physics (stepPhysics, tickVFX, updateProjectiles)
import Save.Codec
import Sim.Rules (applyIntents, checkRules, drainEvents, useItemById, equipById,
                  unequipBySlot, runWorldCommand)
import Sim.Spawn (spawnLevel, respawnPlayer, capturePlayer, applyPlayer)
import World.Level
import World.Scene (SceneDef, loadSceneFile)
import World.Tilemap (mapPixelHeight)

-- | Translate pending SDL events into (quit?, updated raw input).
processEvents :: [SDL.Event] -> RawInput -> (Bool, RawInput)
processEvents events raw0 = foldl step (False, raw0) events
  where
    step (q, raw) event = case SDL.eventPayload event of
      SDL.QuitEvent -> (True, raw)
      SDL.KeyboardEvent kbe ->
        let isDown = SDL.keyboardEventKeyMotion kbe == SDL.Pressed
            keyCode = SDL.keysymKeycode (SDL.keyboardEventKeysym kbe)
        in case keyCode of
             SDL.KeycodeLeft   -> (q, raw { rawLeft      = isDown })
             SDL.KeycodeRight  -> (q, raw { rawRight     = isDown })
             SDL.KeycodeUp     -> (q, raw { rawUp        = isDown })
             SDL.KeycodeDown   -> (q, raw { rawDown      = isDown })
             SDL.KeycodeSpace  -> (q, raw { rawJump      = isDown })
             SDL.KeycodeZ      -> (q, raw { rawAttack    = isDown })
             SDL.KeycodeX      -> (q, raw { rawPick      = isDown })
             SDL.KeycodeI      -> (q, raw { rawBackpack  = isDown })
             SDL.KeycodeA      -> (q, raw { rawHook      = isDown })
             SDL.KeycodeQ      -> (q, raw { rawUsePotion = isDown })
             SDL.KeycodeE      -> (q, raw { rawInteract  = isDown })
             SDL.KeycodeEscape -> (q, raw { rawMenu      = isDown })
             _                 -> (q, raw)
      _ -> (q, raw)

-- | A loaded level: its data, its parallax scene, and a freshly populated
--   ECS world.
data LoadedLevel = LoadedLevel
  { llData  :: !LevelData
  , llScene :: !SceneDef
  , llWorld :: !World
  }

-- | Everything the loop needs but that never changes per frame.
data ShellCtx = ShellCtx
  { scNetwork     :: !GameNetwork
  , scRegistry    :: !ItemRegistry
  , scQuests      :: ![QuestDef]
  , scNpcs        :: !(M.Map NpcId NpcDef)
  , scLevelFiles  :: ![FilePath]
  , scLevelRef    :: !(IORef LoadedLevel)
  , scQuitRef     :: !(IORef Bool)
  , scSettingsRef :: !(IORef Settings)
  , scSlotsRef    :: !(IORef (Maybe String, [Maybe String]))
    -- ^ cached save summaries (autosave, manual slots); disk is only read at
    --   startup and after a save/load, never per frame
  , scFrameRef    :: !(IORef (Int, RunStats, QuestLog))
    -- ^ level index, stats and quest log from the latest 'FrameOut', for
    --   save gathering
  , scWindow      :: !SDL.Window
  , scRenderer    :: !SDL.Renderer
  }

-- | Re-read all save-slot summaries into the cache.
refreshSlots :: ShellCtx -> IO ()
refreshSlots ctx = do
  let summarize n = do
        r <- readSlot n
        pure $ case r of
          Just (Right sv) -> Just (slotSummary sv)
          Just (Left _)   -> Just "CORRUPT"
          Nothing         -> Nothing
  auto <- summarize 0
  manual <- mapM summarize [1, 2, 3]
  writeIORef (scSlotsRef ctx) (auto, manual)

-- | Create a fresh world for a level (including its NPCs). Worlds are cheap;
--   replacing the whole world is how level switching avoids entity-cleanup
--   bugs.
instantiate :: M.Map NpcId NpcDef -> LevelData -> IO LoadedLevel
instantiate npcDefs lvl = do
  world <- initWorld
  runSystem (spawnLevel lvl) world
  runSystem (spawnNpcsForLevel npcDefs (T.pack (ldName lvl))) world
  scene <- loadSceneFile ("assets/scenes/" <> ldName lvl <> ".scene")
             >>= either fail pure
  pure (LoadedLevel lvl scene world)

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "pureHask" SDL.defaultWindow
    { SDL.windowInitialSize = V2 (round screenWidth) (round screenHeight) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    { SDL.rendererType          = SDL.AcceleratedVSyncRenderer
    , SDL.rendererTargetTexture = False
    }

  -- Load the item database, then cross-validate every level file against it
  -- BEFORE the game starts: a typo in assets aborts the launch with a clear
  -- message instead of surfacing mid-play.
  registry <- loadRegistry "assets/items/items.def" >>= either fail pure

  levelFiles <- discoverLevels
  when (null levelFiles) $
    fail ("no level files found in " <> levelDirectory)
  mapM_ (validateLevel registry) levelFiles

  -- Quest and NPC scripts: compile, then cross-validate every reference
  -- (items, quest ids, level names) before the game starts.
  quests <- loadQuestDir "assets/quests" >>= either fail pure
  npcList <- loadNpcDir "assets/npcs" >>= either fail pure
  levelNames <- mapM (fmap (either (const "") ldName) . loadLevelFile) levelFiles
  let questIds = map qdId quests
      npcDefs = M.fromList [ (ndId nd, nd) | nd <- npcList ]
      validation = do
        mapM_ (\qd -> mapM_ (requireItem registry "quest") (questItemRefs qd)) quests
        mapM_ (\nd -> mapM_ (requireItem registry "npc") (npcItemRefs nd)) npcList
        mapM_ (\nd -> mapM_ (\q ->
                 if q `elem` questIds
                   then Right ()
                   else Left ("npc references unknown quest " <> show q))
               (npcQuestRefs nd)) npcList
        mapM_ (\nd ->
                 if ndLevel nd `elem` map (\n -> T.pack n) levelNames
                   then Right ()
                   else Left ("npc '" <> show (ndId nd)
                              <> "' spawns in unknown level " <> show (ndLevel nd)))
              npcList
  case validation of
    Left err -> fail ("script validation failed: " <> err)
    Right () -> pure ()

  settings <- loadSettings
  applyDisplaySettings window settings

  network <- buildNetwork (length levelFiles) quests npcDefs

  -- Load the first level immediately so the title screen has a scene behind it.
  first <- loadLevel npcDefs levelFiles 0
  levelRef <- newIORef first
  quitRef <- newIORef False
  settingsRef <- newIORef settings
  slotsRef <- newIORef (Nothing, [Nothing, Nothing, Nothing])
  frameRef <- newIORef (0, emptyRunStats, emptyQuestLog)

  let ctx = ShellCtx
        { scNetwork = network, scRegistry = registry, scQuests = quests
        , scNpcs = npcDefs, scLevelFiles = levelFiles
        , scLevelRef = levelRef, scQuitRef = quitRef, scSettingsRef = settingsRef
        , scSlotsRef = slotsRef, scFrameRef = frameRef
        , scWindow = window, scRenderer = renderer
        }

  refreshSlots ctx
  startTime <- SDL.ticks
  gameLoop ctx startTime emptyRawInput 0.0 [] 60.0

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

-- | Startup validation: every item marker in a level must exist in the item
--   registry.
validateLevel :: ItemRegistry -> FilePath -> IO ()
validateLevel registry path = do
  parsed <- loadLevelFile path
  case parsed of
    Left err  -> fail ("level validation failed: " <> err)
    Right lvl ->
      case mapM (requireItem registry (ldName lvl) . snd) (ldItems lvl) of
        Left err -> fail ("level validation failed: " <> err)
        Right _  -> pure ()

-- | Load and instantiate a level by index; invalid level files abort with the
--   parser's error message.
loadLevel :: M.Map NpcId NpcDef -> [FilePath] -> Int -> IO LoadedLevel
loadLevel npcDefs files ix = do
  parsed <- loadLevelFile (files !! ix)
  case parsed of
    Left err  -> fail ("level load failed: " <> err)
    Right lvl -> instantiate npcDefs lvl

applyDisplaySettings :: SDL.Window -> Settings -> IO ()
applyDisplaySettings window s =
  SDL.setWindowMode window
    (if setFullscreen s then SDL.FullscreenDesktop else SDL.Windowed)

-- | Execute the flow layer's commands.
runCommand :: ShellCtx -> FlowCommand -> IO ()
runCommand ctx cmd = case cmd of
  CmdNewGame ->
    loadLevel (scNpcs ctx) (scLevelFiles ctx) 0 >>= writeIORef (scLevelRef ctx)
  CmdLoadLevel ix -> do
    -- Carry the persistent player state (backpack, gear, vitals) over into
    -- the freshly built world of the next level.
    LoadedLevel _ _ oldWorld <- readIORef (scLevelRef ctx)
    mPersist <- runSystem capturePlayer oldWorld
    loaded@(LoadedLevel _ _ newWorld) <- loadLevel (scNpcs ctx) (scLevelFiles ctx) ix
    case mPersist of
      Just persist -> runSystem (applyPlayer persist) newWorld
      Nothing      -> pure ()
    writeIORef (scLevelRef ctx) loaded
  CmdSaveGame slot -> do
    LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
    mPersist <- runSystem capturePlayer world
    (levelIx, stats, qlog) <- readIORef (scFrameRef ctx)
    case mPersist of
      Nothing -> pure ()
      Just persist -> do
        writeSlot slot (SaveGame currentSaveVersion levelIx stats persist qlog)
        refreshSlots ctx
  CmdLoadGame slot -> do
    result <- readSlot slot
    case result of
      Just (Right sv) -> do
        loaded@(LoadedLevel _ _ newWorld) <-
          loadLevel (scNpcs ctx) (scLevelFiles ctx) (svLevel sv)
        runSystem (applyPlayer (svPlayer sv)) newWorld
        writeIORef (scLevelRef ctx) loaded
        -- Tell the flow and quest machines to adopt the loaded progress.
        out <- netEvents (scNetwork ctx)
                 [EvRunRestored (svLevel sv) (svStats sv) (svQuests sv)]
        mapM_ (runCommand ctx) (eoCommands out)
      _ -> pure ()  -- empty or corrupt slot: the menu already shows it
  CmdRespawnPlayer -> do
    LoadedLevel lvl _ world <- readIORef (scLevelRef ctx)
    runSystem (respawnPlayer (ldPlayerSpawn lvl)) world
  CmdUseItem iid -> do
    LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
    runSystem (useItemById (scRegistry ctx) iid) world
  CmdEquip iid -> do
    LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
    runSystem (equipById (scRegistry ctx) iid) world
  CmdUnequip slot -> do
    LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
    runSystem (unequipBySlot (scRegistry ctx) slot) world
  CmdToggleSetting ix -> do
    s <- readIORef (scSettingsRef ctx)
    let s' = toggleSetting ix s
    writeIORef (scSettingsRef ctx) s'
    saveSettings s'
    applyDisplaySettings (scWindow ctx) s'
  CmdQuit -> writeIORef (scQuitRef ctx) True

-- | Snapshot the world data the menu can act on.
buildMenuEnv :: ShellCtx -> IO MenuEnv
buildMenuEnv ctx = do
  LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
  backpacks <- runSystem (cfold (\acc (Player, Backpack b) -> b : acc) []) world
  equippedL <- runSystem (cfold (\acc (Player, Equipped e) -> e : acc) []) world
  settings <- readIORef (scSettingsRef ctx)
  (auto, manual) <- readIORef (scSlotsRef ctx)
  let rows = case backpacks of
        (b : _) -> backpackRows (scRegistry ctx) b
        []      -> []
      equipped = case equippedL of
        (e : _) -> e
        []      -> mempty
  pure MenuEnv
    { meBackpack  = rows
    , meEquipped  = [ (slot, M.lookup slot equipped) | slot <- [minBound .. maxBound] ]
    , meSaveSlots = manual
    , meAutoSave  = auto
    , meSettings  = settingsRows settings
    }

-- | Snapshot the world facts the script layer may read (dialogue conditions).
buildWorldSnapshot :: ShellCtx -> IO WorldSnapshot
buildWorldSnapshot ctx = do
  LoadedLevel lvl _ world <- readIORef (scLevelRef ctx)
  backpacks <- runSystem (cfold (\acc (Player, Backpack b) -> b : acc) []) world
  positions <- runSystem
    (cfold (\acc (Player, Position p, Collider s) -> (p + s / 2.0) : acc) []) world
  pure WorldSnapshot
    { wsBackpack  = case backpacks of { (b : _) -> b; [] -> M.empty }
    , wsPlayerPos = case positions of { (p : _) -> p; [] -> V2 0.0 0.0 }
    , wsLevelName = T.pack (ldName lvl)
    }

-- | Main frame loop. @pending@ carries intents that have not yet been
--   consumed by a simulation sub-step; @fps@ is an exponential moving
--   average for the optional FPS display.
gameLoop :: ShellCtx -> Word32 -> RawInput -> Double -> [Intent] -> Double -> IO ()
gameLoop ctx lastTime rawInput acc pending fps = do
  let renderer = scRenderer ctx
  currentTime <- SDL.ticks
  let dt = fromIntegral (currentTime - lastTime) / 1000.0
      fps' = if dt > 0.0 then fps * 0.95 + (1.0 / dt) * 0.05 else fps

  events <- SDL.pollEvents
  let (sdlQuit, rawInput') = processEvents events rawInput

  -- 1. Fire the frame into the network: intents + flow state out.
  menuEnv <- buildMenuEnv ctx
  worldSnap <- buildWorldSnapshot ctx
  frame <- netFrame (scNetwork ctx) dt rawInput' menuEnv worldSnap
  writeIORef (scFrameRef ctx) (foLevel frame, foStats frame, foQuests frame)
  mapM_ (runCommand ctx) (foCommands frame)

  -- 2. Simulate (fixed timestep) only while playing.
  (acc', pending') <- case foMode frame of
    ModePlaying -> do
      LoadedLevel lvl _ world <- readIORef (scLevelRef ctx)
      let tilemap = ldTilemap lvl
          allIntents = pending ++ fiIntents (foInput frame)
          held = fiHeld (foInput frame)
          accTotal = min maxFrameTime (acc + dt)
          steps = floor (accTotal / simStep) :: Int
          accRest = accTotal - fromIntegral steps * simStep

      remaining <- foldM
        (\intents _ -> do
            runSystem (do
              controlPlayer (FrameInput intents held) simStep
              stepNpcs (scNpcs ctx) simStep
              stepPhysics tilemap simStep
              tickVFX simStep
              updateProjectiles tilemap simStep
              ) world
            -- Intents are one-shot: only the first sub-step sees them.
            return [])
        allIntents
        [1 .. steps]

      -- Frame-rate-independent rules run once per frame.
      runSystem (do
        applyIntents (scRegistry ctx) (foInput frame)
        when (IntentInteract `elem` fiIntents (foInput frame)) interactNearest
        checkRules (ldGoal lvl) (mapPixelHeight tilemap)
        ) world

      -- 3. Report simulation facts to the logic layer; execute whatever the
      --    flow and quest machines decided.
      evs <- runSystem drainEvents world
      unless (null evs) $ do
        out <- netEvents (scNetwork ctx) evs
        runSystem
          (mapM_ (\wc -> runWorldCommand wc >> runNpcCommand (scNpcs ctx) wc)
                 (eoWorldCmds out))
          world
        mapM_ (runCommand ctx) (eoCommands out)

      return (accRest, remaining)

    -- Any non-playing mode: freeze the simulation and drop stale time.
    _ -> return (0.0, [])

  -- 4. Render: scene, in-game overlays, mode overlay.
  LoadedLevel lvl scene world <- readIORef (scLevelRef ctx)
  runSystem
    (renderScene renderer (scRegistry ctx) (scNpcs ctx) (ldTilemap lvl) scene)
    world
  when (foMode frame == ModePlaying) $ do
    let tracker = activeGoal (scQuests ctx) (foQuests frame)
    runSystem (renderHud renderer (foToasts frame) tracker) world
    runSystem (renderBackpack renderer (scRegistry ctx)) world
  when (foMode frame == ModeMenu) $
    runSystem
      (renderMenu renderer (scRegistry ctx) (ldTilemap lvl)
         (foMenu frame) menuEnv (foStats frame) (foLevel frame)
         (scQuests ctx) (foQuests frame))
      world
  renderOverlay renderer (foMode frame) (foStats frame) (foLevel frame)

  settings <- readIORef (scSettingsRef ctx)
  when (setShowFps settings) $
    drawText renderer (V4 120 150 190 220) 2.0 (V2 (screenWidth - 90.0) 34.0)
      ("FPS " <> show (round fps' :: Int))

  SDL.present renderer

  quitCmd <- readIORef (scQuitRef ctx)
  unless (sdlQuit || quitCmd) $
    gameLoop ctx currentTime rawInput' acc' pending' fps'
