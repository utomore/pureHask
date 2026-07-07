-- | The Reflex event network — the only module that talks to Reflex.
--
--   Reflex owns exactly two jobs here (see docs/ARCHITECTURE.md):
--
--     1. INPUT SEMANTICS: fold "Input.Semantics" over the per-frame stream of
--        raw key snapshots, producing 'Intent's.
--     2. GAME LOGIC: fold "Game.Logic" (flow + quests + HUD toasts, each a
--        pure machine) over frame ticks and simulation 'GameEvent's, owning
--        'GameMode' / 'RunStats' / 'QuestLog' and emitting 'FlowCommand's
--        and 'WorldCommand's.
--
--   Both folds delegate to pure step functions; this module contains wiring
--   only. Reflex must never reach into the ECS world, and simulation code
--   must never see Reflex types — 'GameNetwork' is the entire surface.
--
--   Definitions (quests, NPC dialogue, engine strings) are read from the
--   shared 'GameDefs' ref AT FIRE TIME and travel inside each firing's
--   payload, so a hot reload takes effect on the very next frame without
--   rebuilding the network.
module FRP.Network
  ( GameNetwork(..)
  , FrameOut(..)
  , EventsOut(..)
  , buildNetwork
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity (Identity(..))
import Data.IORef (IORef, readIORef)
import Data.Text (Text)
import Reflex
import Reflex.Host.Class

import Core.Types
import Flow.Machine
import Game.Assets (GameDefs(..))
import Game.Logic
import Hud.Machine (visibleToasts)
import Input.Semantics

-- | Result of firing one frame into the network (before simulation).
data FrameOut = FrameOut
  { foInput    :: !FrameInput     -- ^ intents + held keys for the simulation
  , foMode     :: !GameMode
  , foStats    :: !RunStats
  , foLevel    :: !Int            -- ^ current level index
  , foMenu     :: !MenuCursor     -- ^ where the menu cursor sits
  , foQuests   :: !QuestLog       -- ^ for the HUD tracker and menu page
  , foToasts   :: ![Text]         -- ^ HUD toasts to draw this frame
  , foCommands :: ![FlowCommand]  -- ^ execute these before simulating
  } deriving (Eq, Show)

-- | Result of firing the simulation's events into the network (after
--   simulation).
data EventsOut = EventsOut
  { eoMode      :: !GameMode
  , eoCommands  :: ![FlowCommand]
  , eoWorldCmds :: ![WorldCommand]
  } deriving (Eq, Show)

-- | Opaque handle used by the main loop.
data GameNetwork = GameNetwork
  { netFrame  :: Double -> RawInput -> MenuEnv -> WorldSnapshot -> IO FrameOut
  , netEvents :: [GameEvent] -> IO EventsOut
  }

-- | Build the network around the shared definitions ref. @levelCount@ fixes
--   when the flow machine rolls the credits.
buildNetwork :: IORef GameDefs -> Int -> IO GameNetwork
buildNetwork defsRef levelCount = do
  defs0 <- readIORef defsRef
  runSpiderHost $ do
    (frameEvent, frameTriggerRef) <- newEventWithTriggerRef
    (simEvent,   simTriggerRef)   <- newEventWithTriggerRef

    (intentDyn, logicDyn) <- runHostFrame $ do
      -- Job 1: input semantics. State carries (tracker, dt, frame input,
      -- menu env, world, defs) so the logic fold below can reuse them from
      -- the same firing.
      intentD <- foldDyn
        (\(dt, raw, env, world, defs) (trk, _, _, _, _, _) ->
           let (trk', fi) = stepIntents dt raw trk
           in (trk', dt, fi, env, world, defs))
        (initialTracker, 0.0, emptyFrameInput, emptyMenuEnv,
         emptyWorldSnapshot, defs0)
        frameEvent

      -- Job 2: game logic. Frame ticks and simulation events feed the same
      -- pure machine; they arrive in separate firings, so 'leftmost' is
      -- safe. Each stimulus carries the defs it was fired with.
      let frameFlowE = (\(_, dt, fi, env, world, defs) ->
                          (defs, FlowFrame dt (fiIntents fi) env world))
                         <$> updated intentD
          logicInE = leftmost
            [ frameFlowE
            , (\(evs, defs) -> (defs, FlowEvents evs)) <$> simEvent
            ]
      logicD <- foldDyn
        (\(defs, flowIn) (ls, _) ->
           stepLogic (gdQuestText defs) (gdQuests defs) (gdNpcs defs) flowIn ls)
        (initialLogic (gdQuests defs0) levelCount, LogicOut [] [])
        logicInE
      return (intentD, logicD)

    intentHandle <- subscribeEvent (updated intentDyn)
    logicHandle  <- subscribeEvent (updated logicDyn)

    let readLogic = do
          mLogic <- readEvent logicHandle
          case mLogic of
            Just readVal -> Just <$> readVal
            Nothing      -> return Nothing

        fallbackLogic = (initialLogic (gdQuests defs0) levelCount, LogicOut [] [])

        fireFrame dt raw env world = runSpiderHost $ do
          defs <- liftIO (readIORef defsRef)
          mTrigger <- liftIO $ readIORef frameTriggerRef
          case mTrigger of
            Nothing ->
              let (ls, _) = fallbackLogic
              in return (frameOutOf emptyFrameInput ls [])
            Just trigger ->
              fireEventsAndRead [trigger :=> Identity (dt, raw, env, world, defs)] $ do
                mIntent <- readEvent intentHandle
                fi <- case mIntent of
                  Just readVal -> (\(_, _, x, _, _, _) -> x) <$> readVal
                  Nothing      -> return emptyFrameInput
                mLogic <- readLogic
                let (ls, out) = maybe fallbackLogic id mLogic
                return (frameOutOf fi ls (loFlowCmds out))

        fireSimEvents evs = runSpiderHost $ do
          defs <- liftIO (readIORef defsRef)
          mTrigger <- liftIO $ readIORef simTriggerRef
          case mTrigger of
            Nothing -> return (EventsOut ModeTitle [] [])
            Just trigger ->
              fireEventsAndRead [trigger :=> Identity (evs, defs)] $ do
                mLogic <- readLogic
                let (ls, out) = maybe fallbackLogic id mLogic
                return EventsOut
                  { eoMode      = fsMode (lsFlow ls)
                  , eoCommands  = loFlowCmds out
                  , eoWorldCmds = loWorldCmds out
                  }

    return (GameNetwork fireFrame fireSimEvents)
  where
    frameOutOf fi ls cmds = FrameOut
      { foInput    = fi
      , foMode     = fsMode (lsFlow ls)
      , foStats    = fsStats (lsFlow ls)
      , foLevel    = fsLevel (lsFlow ls)
      , foMenu     = fsMenu (lsFlow ls)
      , foQuests   = lsQuests ls
      , foToasts   = visibleToasts (lsHud ls)
      , foCommands = cmds
      }
