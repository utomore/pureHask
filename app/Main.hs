-- | The effectful shell: SDL window, the frame loop, and execution of
--   'FlowCommand's. This module wires the layers together but decides
--   nothing itself:
--
--     * raw SDL events   -> 'RawInput'                (translation only)
--     * 'RawInput'       -> "FRP.Network" 'netFrame'  (intents, mode, commands)
--     * intents          -> fixed-step simulation     (only in 'ModePlaying')
--     * simulation facts -> "FRP.Network" 'netEvents' (mode reacts)
--     * ECS world        -> "Render.Draw" / "Render.UI" / "Render.Menu"
--
--   Definition assets live in one hot-swappable 'GameDefs' bundle (see
--   "Game.Assets"): the shell polls file timestamps about once a second and
--   reloads the bundle in place — a broken edit keeps the old definitions
--   and prints the error. @--validate@ runs the same pipeline headless.
module Main where

import Control.Monad (unless, when, filterM, foldM, forM, forM_)
import Data.IORef
import Data.List (elemIndex, isSuffixOf, sort)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Word (Word32)
import Apecs (runSystem, cfold)
import SDL (($=))
import qualified SDL
import Linear (V2(..), V4(..))
import System.Directory (doesDirectoryExist, doesFileExist, getModificationTime,
                         listDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))

import Audio.Player (AudioState, initAudio, loadAudioState, destroyAudioState,
                     shutdownAudio, playEventSfx, syncMusic)
import Audio.Script (adSfx, adMusic)
import Core.Components (World, initWorld, Player(..), Backpack(..), Equipped(..),
                        Position(..), Collider(..), Talents(..))
import Core.Config
import Core.Settings
import Core.Types
import FRP.Network
import Game.Assets (GameDefs(..), loadDefs)
import Items.Registry (allItems, defId, requireItem)
import Quest.Runtime (activeGoal)
import Quest.Script (QuestDef(..))
import Render.Draw (renderScene)
import Render.Font (FontSet(..), Fonts, loadFonts, freeFonts, drawText)
import Render.Hud (renderHud)
import Render.Menu (renderMenu)
import Render.Sprites (SpriteTextures, loadSpriteTextures, destroySpriteTextures)
import Render.UI (renderOverlay, renderBackpack)
import Sim.Combat (controlPlayer)
import Sim.Damage (resolveDamage)
import Sim.Enemy (spawnEnemiesForLevel, stepEnemies)
import Sim.Items (backpackRows)
import Sim.Npc (spawnNpcsForLevel, stepNpcs, runNpcCommand, interactNearest)
import Sim.Particles (tickParticles)
import Sim.Physics (stepPhysics, tickVFX, updateProjectiles)
import Save.Codec
import Sim.Rules (applyIntents, checkRules, drainEvents, useItemById, equipById,
                  unequipBySlot, learnTalentById, refreshPlayerStats,
                  runWorldCommand)
import Sim.Spawn (spawnLevel, respawnPlayer, capturePlayer, applyPlayer)
import Talent.Core (talentRows)
import Talent.Script (TalentDef(..))
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

-- | Everything the loop needs. Definitions sit behind an 'IORef' so a hot
--   reload swaps them for the whole game (shell, simulation and network) at
--   a frame boundary.
data ShellCtx = ShellCtx
  { scNetwork     :: !GameNetwork
  , scDefsRef     :: !(IORef GameDefs)
  , scLevelFiles  :: ![FilePath]
  , scLevelRef    :: !(IORef LoadedLevel)
  , scQuitRef     :: !(IORef Bool)
  , scSettingsRef :: !(IORef Settings)
  , scSaveEnv     :: !SaveEnv
    -- ^ level names for the save codec (v2 stores names, not indices)
  , scSlotsRef    :: !(IORef (Maybe String, [Maybe String]))
    -- ^ cached save summaries (autosave, manual slots); disk is only read at
    --   startup and after a save/load, never per frame
  , scFrameRef    :: !(IORef (Int, RunStats, QuestLog))
    -- ^ level index, stats and quest log from the latest 'FrameOut', for
    --   save gathering
  , scWatchRef    :: !(IORef (Word32, [(FilePath, UTCTime)], [(FilePath, UTCTime)]))
    -- ^ hot reload: next poll time (SDL ticks), last seen definition-file
    --   mtimes, last seen level/scene-file mtimes
  , scWindow      :: !SDL.Window
  , scRenderer    :: !SDL.Renderer
  , scTexturesRef :: !(IORef SpriteTextures)
    -- ^ sprite-sheet textures; rebuilt when a sprite definition or sheet
    --   image changes on disk
  , scAudioRef    :: !(IORef (Maybe AudioState))
    -- ^ loaded sounds/tracks; 'Nothing' when no audio device is available
  , scFonts       :: !Fonts
    -- ^ TTF fonts loaded at startup; the pixel font needs no handle. The
    --   active choice is read from the settings each frame.
  }

-- | Re-read all save-slot summaries into the cache.
refreshSlots :: ShellCtx -> IO ()
refreshSlots ctx = do
  let summarize n = do
        r <- readSlot (scSaveEnv ctx) n
        pure $ case r of
          Just (Right sv) -> Just (slotSummary sv)
          Just (Left _)   -> Just "CORRUPT"
          Nothing         -> Nothing
  auto <- summarize 0
  manual <- mapM summarize [1, 2, 3]
  writeIORef (scSlotsRef ctx) (auto, manual)

-- | Create a fresh world for a level (including its NPCs and enemies).
--   Worlds are cheap; replacing the whole world is how level switching
--   avoids entity-cleanup bugs.
tryInstantiate :: GameDefs -> LevelData -> IO (Either String LoadedLevel)
tryInstantiate defs lvl = do
  sceneR <- loadSceneFile ("assets/scenes/" <> ldName lvl <> ".scene")
  case sceneR of
    Left err -> pure (Left err)
    Right scene -> do
      world <- initWorld
      runSystem (spawnLevel lvl) world
      runSystem (spawnNpcsForLevel (gdNpcs defs) (T.pack (ldName lvl))) world
      runSystem (spawnEnemiesForLevel (gdEnemies defs) (T.pack (ldName lvl))) world
      pure (Right (LoadedLevel lvl scene world))

instantiate :: GameDefs -> LevelData -> IO LoadedLevel
instantiate defs lvl = tryInstantiate defs lvl >>= either fail pure

-- | Discover levels and their base names; fails on an empty set or a level
--   that does not even parse.
discoverLevelSet :: IO ([FilePath], [String])
discoverLevelSet = do
  levelFiles <- discoverLevels
  when (null levelFiles) $
    fail ("no level files found in " <> levelDirectory)
  levelNames <- forM levelFiles $ \path ->
    loadLevelFile path >>= either fail (pure . ldName)
  pure (levelFiles, levelNames)

main :: IO ()
main = do
  -- Line-buffer the log so hot-reload / sanitize messages show up promptly
  -- even when stdout is a pipe.
  hSetBuffering stdout LineBuffering
  args <- getArgs
  if "--validate" `elem` args
    then runValidate
    else runGame

-- | Headless validation: run the full asset pipeline for EVERY language and
--   exit non-zero on the first problem. For content authors and CI.
runValidate :: IO ()
runValidate = do
  (levelFiles, levelNames) <- discoverLevelSet
  putStrLn ("levels: " <> show (length levelFiles) <> " OK")
  results <- forM [minBound .. maxBound :: Language] $ \lang -> do
    r <- loadDefs lang levelFiles levelNames
    case r of
      Left err -> do
        putStrLn ("[" <> langCode lang <> "] FAILED: " <> err)
        pure False
      Right defs -> do
        putStrLn ("[" <> langCode lang <> "] OK: "
                  <> show (length (allItems (gdRegistry defs))) <> " items, "
                  <> show (length (gdQuests defs)) <> " quests, "
                  <> show (M.size (gdNpcs defs)) <> " npcs, "
                  <> show (M.size (gdEnemies defs)) <> " enemies, "
                  <> show (length (gdTalents defs)) <> " talents, "
                  <> show (M.size (gdSprites defs)) <> " sprites, "
                  <> show (M.size (adSfx (gdAudio defs))) <> " sfx, "
                  <> show (M.size (adMusic (gdAudio defs))) <> " tracks")
        pure True
  if and results then exitSuccess else exitFailure

runGame :: IO ()
runGame = do
  SDL.initializeAll
  -- Linear filtering when the logical 800x600 canvas is scaled to a resized
  -- window; solid-colour rects are unaffected, text stays smooth.
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  window <- SDL.createWindow "pureHask" SDL.defaultWindow
    { SDL.windowInitialSize = V2 (round screenWidth) (round screenHeight)
    , SDL.windowResizable   = True
    }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    { SDL.rendererType          = SDL.AcceleratedVSyncRenderer
    , SDL.rendererTargetTexture = False
    }
  -- The game always draws in its logical 800x600 coordinate space; SDL
  -- scales it to whatever size the (resizable) window currently has.
  SDL.rendererLogicalSize renderer $=
    Just (V2 (round screenWidth) (round screenHeight))

  fonts <- loadFonts "assets/fonts/NotoSansTC.ttf"

  -- Settings first: the chosen language decides which string table every
  -- content compiler resolves text keys against. All loading + cross
  -- validation lives in Game.Assets (shared with hot reload / --validate).
  settings <- loadSettings
  (levelFiles, levelNames) <- discoverLevelSet
  defs0 <- loadDefs (setLang settings) levelFiles levelNames
             >>= either fail pure
  defsRef <- newIORef defs0

  applyDisplaySettings window settings

  -- Sprite sheets were validated by loadDefs; a load failure here means a
  -- genuinely broken image and aborts, like any other bad asset.
  textures0 <- loadSpriteTextures renderer "assets/textures"
                 (M.elems (gdSprites defs0))
                 >>= either fail pure
  texturesRef <- newIORef textures0

  -- Audio: a machine without a device plays silent; a broken sound FILE at
  -- startup aborts like any other bad asset (references were validated).
  audioDevice <- initAudio
  audio0 <- case audioDevice of
    Nothing -> pure Nothing
    Just () -> Just <$> (loadAudioState "assets/audio" (gdAudio defs0)
                           >>= either fail pure)
  audioRef <- newIORef audio0

  network <- buildNetwork defsRef (length levelFiles)

  -- Load the first level immediately so the title screen has a scene behind it.
  first <- loadLevelIx defs0 levelFiles 0
  levelRef <- newIORef first
  quitRef <- newIORef False
  settingsRef <- newIORef settings
  slotsRef <- newIORef (Nothing, [Nothing, Nothing, Nothing])
  frameRef <- newIORef (0, emptyRunStats, emptyQuestLog)
  mtimes0 <- watchedMtimes (setLang settings)
  lvlMtimes0 <- watchedLevelMtimes
  watchRef <- newIORef (0, mtimes0, lvlMtimes0)

  let ctx = ShellCtx
        { scNetwork = network, scDefsRef = defsRef, scLevelFiles = levelFiles
        , scSaveEnv = SaveEnv levelNames
        , scLevelRef = levelRef, scQuitRef = quitRef, scSettingsRef = settingsRef
        , scSlotsRef = slotsRef, scFrameRef = frameRef, scWatchRef = watchRef
        , scWindow = window, scRenderer = renderer
        , scTexturesRef = texturesRef, scAudioRef = audioRef, scFonts = fonts
        }

  refreshSlots ctx
  startTime <- SDL.ticks
  gameLoop ctx startTime emptyRawInput 0.0 [] 60.0

  readIORef audioRef >>= mapM_ destroyAudioState
  when (audioDevice /= Nothing) shutdownAudio
  readIORef texturesRef >>= destroySpriteTextures
  freeFonts fonts
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

-- | Load and instantiate a level by index; invalid level files abort with the
--   parser's error message.
loadLevelIx :: GameDefs -> [FilePath] -> Int -> IO LoadedLevel
loadLevelIx defs files ix = do
  parsed <- loadLevelFile (files !! ix)
  case parsed of
    Left err  -> fail ("level load failed: " <> err)
    Right lvl -> instantiate defs lvl

applyDisplaySettings :: SDL.Window -> Settings -> IO ()
applyDisplaySettings window s =
  SDL.setWindowMode window
    (if setFullscreen s then SDL.FullscreenDesktop else SDL.Windowed)

--------------------------------------------------------------------------------
-- Hot reload
--------------------------------------------------------------------------------

-- | Every definition file the hot reload watches. Levels and scenes are
--   deliberately absent: changing them mid-run means rebuilding worlds.
watchedMtimes :: Language -> IO [(FilePath, UTCTime)]
watchedMtimes lang = do
  let dirs = [ "assets/quests", "assets/npcs", "assets/enemies"
             , "assets/sprites", "assets/textures"
             , "assets/audio/sfx", "assets/audio/music" ]
      singles = [ "assets/items/items.def"
                , "assets/talents/talents.def"
                , "assets/audio/audio.def"
                , "assets/lang" </> (langCode lang <> ".lang")
                ]
  dirFiles <- fmap concat . forM dirs $ \dir -> do
    exists <- doesDirectoryExist dir
    if exists
      then map (dir </>) <$> listDirectory dir
      else pure []
  -- The talent file is optional (see Talent.Script); only watch what exists.
  present <- filterM doesFileExist (singles <> dirFiles)
  forM present $ \path -> do
    t <- getModificationTime path
    pure (path, t)

-- | The level and scene files (for rebuilding the CURRENT level on edit).
watchedLevelMtimes :: IO [(FilePath, UTCTime)]
watchedLevelMtimes = do
  let dirs = [(levelDirectory, ".txt"), ("assets/scenes", ".scene")]
  files <- fmap concat . forM dirs $ \(dir, ext) -> do
    exists <- doesDirectoryExist dir
    if exists
      then map (dir </>) . sort . filter (ext `isSuffixOf`)
             <$> listDirectory dir
      else pure []
  forM files $ \path -> do
    t <- getModificationTime path
    pure (path, t)

-- | Poll the watched files about once a second; on any change re-run the
--   relevant pipeline. A broken edit keeps the old data (and says so) —
--   content iteration must never crash a running game.
checkHotReload :: ShellCtx -> Word32 -> IO ()
checkHotReload ctx now = do
  (nextAt, defsBefore, lvlBefore) <- readIORef (scWatchRef ctx)
  when (now >= nextAt) $ do
    settings <- readIORef (scSettingsRef ctx)
    defsAfter <- watchedMtimes (setLang settings)
    lvlAfter <- watchedLevelMtimes
    writeIORef (scWatchRef ctx) (now + 1000, defsAfter, lvlAfter)
    when (defsAfter /= defsBefore) $ do
      result <- loadDefs (setLang settings) (scLevelFiles ctx)
                  (seLevelNames (scSaveEnv ctx))
      case result of
        Right defs -> do
          writeIORef (scDefsRef ctx) defs
          -- Talent or item stats may have changed under the player's feet.
          LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
          runSystem (refreshPlayerStats (gdRegistry defs) (gdTalents defs)) world
          -- Sprite sheets travel with the definitions: rebuild the texture
          -- cache; a broken image keeps the old textures (and says so).
          texResult <- loadSpriteTextures (scRenderer ctx) "assets/textures"
                         (M.elems (gdSprites defs))
          case texResult of
            Right texs -> do
              old <- readIORef (scTexturesRef ctx)
              destroySpriteTextures old
              writeIORef (scTexturesRef ctx) texs
            Left err ->
              putStrLn ("[hot-reload] " <> err <> " -- keeping the old textures")
          -- Sounds too; the music halts and the next frame restarts the
          -- right track from the reloaded table.
          mOldAudio <- readIORef (scAudioRef ctx)
          forM_ mOldAudio $ \oldAudio -> do
            audioResult <- loadAudioState "assets/audio" (gdAudio defs)
            case audioResult of
              Right newAudio -> do
                destroyAudioState oldAudio
                writeIORef (scAudioRef ctx) (Just newAudio)
              Left err ->
                putStrLn ("[hot-reload] " <> err <> " -- keeping the old sounds")
          putStrLn "[hot-reload] definitions reloaded"
        Left err ->
          putStrLn ("[hot-reload] " <> err <> " -- keeping the old definitions")
    when (lvlAfter /= lvlBefore) $
      reloadCurrentLevel ctx

-- | A level or scene file changed: rebuild the CURRENT level's world from
--   disk, carrying the player's inventory/vitals over (position resets to
--   the spawn point — the terrain may have changed under their feet).
--   Adding or removing level FILES needs a restart: the level count is
--   baked into the flow machine and the save codec.
reloadCurrentLevel :: ShellCtx -> IO ()
reloadCurrentLevel ctx = do
  files <- discoverLevels
  if files /= scLevelFiles ctx
    then putStrLn "[hot-reload] level files were added/removed -- restart to pick them up"
    else do
      defs <- readIORef (scDefsRef ctx)
      LoadedLevel oldLvl _ oldWorld <- readIORef (scLevelRef ctx)
      let ix = maybe 0 id (elemIndex (ldName oldLvl) (seLevelNames (scSaveEnv ctx)))
      parsed <- loadLevelFile (scLevelFiles ctx !! ix)
      case parsed >>= checkMarkers defs of
        Left err ->
          putStrLn ("[hot-reload] " <> err <> " -- keeping the old level")
        Right lvl -> do
          mPersist <- runSystem capturePlayer oldWorld
          rebuilt <- tryInstantiate defs lvl
          case rebuilt of
            Left err ->
              putStrLn ("[hot-reload] " <> err <> " -- keeping the old level")
            Right loaded@(LoadedLevel _ _ newWorld) -> do
              forM_ mPersist $ \p -> runSystem (applyPlayer p) newWorld
              writeIORef (scLevelRef ctx) loaded
              putStrLn ("[hot-reload] level '" <> ldName lvl <> "' rebuilt")
  where
    checkMarkers defs lvl =
      case mapM (requireItem (gdRegistry defs) (ldName lvl) . snd) (ldItems lvl) of
        Left err -> Left err
        Right _  -> Right lvl

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

-- | Execute the flow layer's commands.
runCommand :: ShellCtx -> FlowCommand -> IO ()
runCommand ctx cmd = do
  defs <- readIORef (scDefsRef ctx)
  case cmd of
    CmdNewGame ->
      loadLevelIx defs (scLevelFiles ctx) 0 >>= writeIORef (scLevelRef ctx)
    CmdLoadLevel ix -> do
      -- Carry the persistent player state (backpack, gear, vitals) over into
      -- the freshly built world of the next level.
      LoadedLevel _ _ oldWorld <- readIORef (scLevelRef ctx)
      mPersist <- runSystem capturePlayer oldWorld
      loaded@(LoadedLevel _ _ newWorld) <- loadLevelIx defs (scLevelFiles ctx) ix
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
          writeSlot (scSaveEnv ctx) slot
            (SaveGame currentSaveVersion levelIx stats persist qlog)
          refreshSlots ctx
    CmdLoadGame slot -> do
      result <- readSlot (scSaveEnv ctx) slot
      case result of
        Just (Right sv0) -> do
          -- Drop/clamp references to content that changed since the save was
          -- written; every adjustment is reported, never silent.
          let knownQuests = [ (qdId qd, length (qdStages qd)) | qd <- gdQuests defs ]
              knownItems = map defId (allItems (gdRegistry defs))
              knownTalents = [ (tdId td, tdMaxRank td, tdCost td)
                             | td <- gdTalents defs ]
              (sv, warns) = sanitizeSave knownQuests knownItems knownTalents sv0
          mapM_ putStrLn warns
          loaded@(LoadedLevel _ _ newWorld) <-
            loadLevelIx defs (scLevelFiles ctx) (svLevel sv)
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
      runSystem (useItemById (gdRegistry defs) iid) world
    CmdEquip iid -> do
      LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
      runSystem (equipById (gdRegistry defs) (gdTalents defs) iid) world
    CmdUnequip slot -> do
      LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
      runSystem (unequipBySlot (gdRegistry defs) (gdTalents defs) slot) world
    CmdLearnTalent tid -> do
      LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
      runSystem (learnTalentById (gdRegistry defs) (gdTalents defs) tid) world
    CmdToggleSetting ix -> do
      s <- readIORef (scSettingsRef ctx)
      let s' = toggleSetting ix s
      writeIORef (scSettingsRef ctx) s'
      saveSettings s'
      applyDisplaySettings (scWindow ctx) s'
    CmdQuit -> writeIORef (scQuitRef ctx) True

-- | Snapshot the world data the menu can act on.
buildMenuEnv :: ShellCtx -> GameDefs -> IO MenuEnv
buildMenuEnv ctx defs = do
  LoadedLevel _ _ world <- readIORef (scLevelRef ctx)
  backpacks <- runSystem (cfold (\acc (Player, Backpack b) -> b : acc) []) world
  equippedL <- runSystem (cfold (\acc (Player, Equipped e) -> e : acc) []) world
  talentsL <- runSystem (cfold (\acc (Player, Talents t) -> t : acc) []) world
  settings <- readIORef (scSettingsRef ctx)
  (auto, manual) <- readIORef (scSlotsRef ctx)
  let rows = case backpacks of
        (b : _) -> backpackRows (gdRegistry defs) b
        []      -> []
      equipped = case equippedL of
        (e : _) -> e
        []      -> mempty
      talents = case talentsL of
        (t : _) -> t
        []      -> emptyTalentState
  pure MenuEnv
    { meBackpack  = rows
    , meEquipped  = [ (slot, M.lookup slot equipped) | slot <- [minBound .. maxBound] ]
    , meTalents   = talentRows (gdTalents defs) talents
    , meTalentPts = tsPoints talents
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

  checkHotReload ctx currentTime
  defs <- readIORef (scDefsRef ctx)

  events <- SDL.pollEvents
  let (sdlQuit, rawInput') = processEvents events rawInput

  -- 1. Fire the frame into the network: intents + flow state out.
  menuEnv <- buildMenuEnv ctx defs
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
              stepNpcs (gdNpcs defs) simStep
              stepEnemies (gdEnemies defs) simStep
              stepPhysics tilemap simStep
              resolveDamage (gdEnemies defs) simStep
              tickVFX simStep
              tickParticles simStep
              updateProjectiles tilemap simStep
              ) world
            -- Intents are one-shot: only the first sub-step sees them.
            return [])
        allIntents
        [1 .. steps]

      -- Frame-rate-independent rules run once per frame.
      runSystem (do
        applyIntents (gdRegistry defs) (foInput frame)
        when (IntentInteract `elem` fiIntents (foInput frame)) interactNearest
        checkRules (ldGoal lvl) (mapPixelHeight tilemap)
        ) world

      -- 3. Report simulation facts to the logic layer; execute whatever the
      --    flow and quest machines decided.
      evs <- runSystem drainEvents world
      unless (null evs) $ do
        -- Event-driven audio: the sim published facts, play their sounds.
        mAudio <- readIORef (scAudioRef ctx)
        forM_ mAudio $ \au -> mapM_ (playEventSfx au (gdAudio defs)) evs
        out <- netEvents (scNetwork ctx) evs
        runSystem
          (mapM_ (\wc -> runWorldCommand (gdRegistry defs) (gdTalents defs) wc
                           >> runNpcCommand (gdNpcs defs) wc)
                 (eoWorldCmds out))
          world
        mapM_ (runCommand ctx) (eoCommands out)

      return (accRest, remaining)

    -- Any non-playing mode: freeze the simulation and drop stale time.
    _ -> return (0.0, [])

  -- 4. Render: scene, in-game overlays, mode overlay. All text goes through
  --    a per-frame FontSet so the FONT setting takes effect immediately.
  settings <- readIORef (scSettingsRef ctx)
  let fonts = FontSet renderer (setFont settings) (scFonts ctx)
  LoadedLevel lvl scene world <- readIORef (scLevelRef ctx)
  textures <- readIORef (scTexturesRef ctx)
  -- Music follows the mode/level the flow machine decided on.
  mAudio <- readIORef (scAudioRef ctx)
  forM_ mAudio $ \au ->
    syncMusic au (gdAudio defs) (foMode frame) (T.pack (ldName lvl))
  let animClock = fromIntegral currentTime / 1000.0
  runSystem
    (renderScene fonts (gdRegistry defs) (gdNpcs defs) (gdEnemies defs)
       (gdSprites defs) textures animClock (ldTilemap lvl) scene)
    world
  when (foMode frame == ModePlaying) $ do
    let tracker = activeGoal (gdQuests defs) (foQuests frame)
    runSystem (renderHud fonts (foToasts frame) tracker) world
    runSystem (renderBackpack fonts (gdRegistry defs)) world
  when (foMode frame == ModeMenu) $
    runSystem
      (renderMenu fonts (gdRegistry defs) (gdTalents defs) (ldTilemap lvl)
         (foMenu frame) menuEnv (foStats frame) (foLevel frame)
         (gdQuests defs) (foQuests frame))
      world
  renderOverlay fonts (foMode frame) (foStats frame) (foLevel frame)

  when (setShowFps settings) $
    drawText fonts (V4 120 150 190 220) 2.0 (V2 (screenWidth - 90.0) 34.0)
      ("FPS " <> show (round fps' :: Int))

  SDL.present renderer

  quitCmd <- readIORef (scQuitRef ctx)
  unless (sdlQuit || quitCmd) $
    gameLoop ctx currentTime rawInput' acc' pending' fps'
