-- | The game-flow state machine: the single owner of 'GameMode', 'RunStats'
--   and the menu cursor.
--
--   It reacts to two kinds of stimuli:
--
--     * 'FlowFrame' — one per rendered frame, carrying dt, the frame's
--       intents and a 'MenuEnv' snapshot (row counts and selectable ids for
--       menu pages)
--     * 'FlowEvents' — facts emitted by the simulation ('GameEvent's)
--
--   and answers with 'FlowCommand's that the effectful shell ("Main")
--   executes. The flow machine decides WHEN things happen; it never touches
--   the ECS world itself.
--
--   Pure step function, folded by "FRP.Network", unit-tested directly.
module Flow.Machine
  ( FlowState(..)
  , FlowIn(..)
  , initialFlow
  , stepFlow
  , openDialogue
  , menuRowCount
  ) where

import Data.Text (Text)

import Core.Config (deathPauseDuration, levelCompleteDuration)
import Core.Types

-- | Flow-layer state. @fsLevelCount@ is fixed at startup from the number of
--   discovered level files.
data FlowState = FlowState
  { fsMode       :: !GameMode
  , fsStats      :: !RunStats
  , fsLevel      :: !Int      -- ^ current level index (0-based)
  , fsLevelCount :: !Int
  , fsMenu       :: !MenuCursor
  } deriving (Eq, Show)

-- | Stimuli the machine consumes.
data FlowIn
  = FlowFrame !Double ![Intent] !MenuEnv !WorldSnapshot
    -- ^ dt, intents, menu snapshot, world snapshot (for script conditions)
  | FlowEvents ![GameEvent]
    -- ^ simulation facts of this frame
  deriving (Eq, Show)

initialFlow :: Int -> FlowState
initialFlow levelCount = FlowState
  { fsMode       = ModeTitle
  , fsStats      = emptyRunStats
  , fsLevel      = 0
  , fsLevelCount = levelCount
  , fsMenu       = initialCursor
  }

-- | Rows the cursor can visit on a page, given the current env.
menuRowCount :: MenuEnv -> MenuPage -> Int
menuRowCount env page = case page of
  PageStatus   -> 0
  PageBackpack -> length (meBackpack env)
  PageEquip    -> length (meEquipped env)
  PageTalents  -> length (meTalents env)
  PageQuests   -> 0
  PageMap      -> 0
  PageSave     -> length (meSaveSlots env)
  PageLoad     -> 1 + length (meSaveSlots env)  -- autosave row + manual slots
  PageSettings -> length (meSettings env)

-- | Advance the flow machine. Returns the new state and the commands the
--   shell must execute this frame.
stepFlow :: FlowIn -> FlowState -> (FlowState, [FlowCommand])
stepFlow (FlowFrame dt intents env _world) fs = case fsMode fs of
  ModeTitle
    | IntentJump `elem` intents ->
        -- Start a fresh run: reset stats, load the first level.
        ( fs { fsMode = ModePlaying, fsStats = emptyRunStats, fsLevel = 0 }
        , [CmdNewGame] )
    | IntentMenu `elem` intents -> (fs, [CmdQuit])
    | otherwise -> (fs, [])

  ModePlaying
    | IntentMenu `elem` intents ->
        (fs { fsMode = ModeMenu, fsMenu = initialCursor }, [])
    | otherwise ->
        let stats = fsStats fs
        in (fs { fsStats = stats { statTime = statTime stats + dt } }, [])

  ModeMenu
    | IntentMenu `elem` intents -> (fs { fsMode = ModePlaying }, [])
    | otherwise -> stepMenu intents env fs

  -- Dialogue: jump advances one line; the last line closes the box.
  ModeDialogue pending
    | IntentJump `elem` intents || IntentInteract `elem` intents ->
        case drop 1 pending of
          []   -> (fs { fsMode = ModePlaying }, [])
          rest -> (fs { fsMode = ModeDialogue rest }, [])
    | IntentMenu `elem` intents -> (fs { fsMode = ModePlaying }, [])
    | otherwise -> (fs, [])

  ModeDead t ->
    let t' = t - dt
    in if t' <= 0.0
         then (fs { fsMode = ModePlaying }, [CmdRespawnPlayer])
         else (fs { fsMode = ModeDead t' }, [])

  ModeLevelComplete t ->
    let t' = t - dt
    in if t' > 0.0
         then (fs { fsMode = ModeLevelComplete t' }, [])
         else
           let next = fsLevel fs + 1
           in if next >= fsLevelCount fs
                then (fs { fsMode = ModeEnding }, [])
                else ( fs { fsMode = ModePlaying, fsLevel = next }
                     -- Load the next level, then autosave (slot 0).
                     , [CmdLoadLevel next, CmdSaveGame 0] )

  ModeEnding
    | IntentJump `elem` intents -> (fs { fsMode = ModeTitle }, [])
    | IntentMenu `elem` intents -> (fs, [CmdQuit])
    | otherwise -> (fs, [])

stepFlow (FlowEvents evs) fs = (foldl react fs evs, [])
  where
    react s ev = case (fsMode s, ev) of
      -- Death and goal transitions only make sense while playing; stale
      -- events arriving in other modes are ignored.
      (ModePlaying, EvPlayerDied) ->
        let stats = fsStats s
        in s { fsMode = ModeDead deathPauseDuration
             , fsStats = stats { statDeaths = statDeaths stats + 1 }
             }
      (ModePlaying, EvGoalReached) ->
        s { fsMode = ModeLevelComplete levelCompleteDuration }
      (_, EvItemPicked _) ->
        let stats = fsStats s
        in s { fsStats = stats { statItems = statItems stats + 1 } }
      (_, EvEnemyKilled _) ->
        let stats = fsStats s
        in s { fsStats = stats { statKills = statKills stats + 1 } }
      -- A save was loaded by the shell: adopt its progress and play. (The
      -- quest log part is consumed by "Quest.Runtime".)
      (_, EvRunRestored level stats _qlog) ->
        s { fsMode = ModePlaying, fsLevel = level, fsStats = stats }
      _ -> s

-- | Enter dialogue mode with the given (speaker, line) pairs. Used by
--   "Game.Logic" when an NPC conversation starts; no-op for empty dialogue.
openDialogue :: [(Text, Text)] -> FlowState -> FlowState
openDialogue [] fs    = fs
openDialogue lns fs   = fs { fsMode = ModeDialogue lns }

--------------------------------------------------------------------------------
-- Menu navigation
--------------------------------------------------------------------------------

stepMenu :: [Intent] -> MenuEnv -> FlowState -> (FlowState, [FlowCommand])
stepMenu intents env fs
  | IntentNavLeft  `elem` intents = (moveTo (cyclePage (-1)), [])
  | IntentNavRight `elem` intents = (moveTo (cyclePage 1), [])
  | IntentNavUp    `elem` intents = (moveRow (-1), [])
  | IntentNavDown  `elem` intents = (moveRow 1, [])
  | IntentJump     `elem` intents = confirm
  | otherwise = (fs, [])
  where
    cursor = fsMenu fs
    page = mcPage cursor

    cyclePage d =
      let n = length menuPages
          ix = fromEnum page
      in toEnum ((ix + d + n) `mod` n)

    moveTo p = fs { fsMenu = MenuCursor p 0 }

    moveRow d =
      let count = menuRowCount env page
          row' = max 0 (min (count - 1) (mcRow cursor + d))
      in fs { fsMenu = cursor { mcRow = max 0 row' } }

    confirm = case page of
      PageBackpack ->
        case drop (mcRow cursor) (meBackpack env) of
          ((iid, CatPotion, _) : _)   -> (fs, [CmdUseItem iid])
          ((iid, CatEquip _, _) : _)  -> (fs, [CmdEquip iid])
          _                           -> (fs, [])
      PageEquip ->
        case drop (mcRow cursor) (meEquipped env) of
          ((slot, Just _) : _) -> (fs, [CmdUnequip slot])
          _                    -> (fs, [])
      -- Only rows the pure rules say are buyable emit a command; the shell
      -- re-checks against the world before spending anything.
      PageTalents ->
        case drop (mcRow cursor) (meTalents env) of
          ((tid, _, True) : _) -> (fs, [CmdLearnTalent tid])
          _                    -> (fs, [])
      PageSettings ->
        if mcRow cursor < length (meSettings env)
          then (fs, [CmdToggleSetting (mcRow cursor)])
          else (fs, [])
      -- Manual save slots are 1-based (slot 0 is the autosave).
      PageSave ->
        if mcRow cursor < length (meSaveSlots env)
          then (fs, [CmdSaveGame (mcRow cursor + 1)])
          else (fs, [])
      -- Load page row 0 is the autosave; only existing slots load.
      PageLoad ->
        let slot = mcRow cursor
            exists = case slot of
              0 -> meAutoSave env /= Nothing
              n -> case drop (n - 1) (meSaveSlots env) of
                     (Just _ : _) -> True
                     _            -> False
        in if exists then (fs, [CmdLoadGame slot]) else (fs, [])
      _ -> (fs, [])
