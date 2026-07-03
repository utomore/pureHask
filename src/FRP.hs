module FRP where

import Control.Monad.IO.Class (liftIO)
import Data.Dependent.Sum (DSum(..))
import Data.Functor.Identity (Identity(..))
import Data.IORef
import Reflex
import Reflex.Host.Class

import Types
import Physics

-- | Triggers to feed external inputs and ticks into Reflex.
data GameTriggers = GameTriggers
  { fireTick  :: !(Double -> IO (Maybe (InputState, Double)))
  , fireInput :: !(InputState -> IO ())
  }

-- | Setup the Reflex network. Returns the triggers and the EventHandle
--   for the combined (InputState, Double) event.
setupReflex :: IO (GameTriggers, EventHandle (SpiderTimeline Global) (InputState, Double))
setupReflex = runSpiderHost $ do
  -- 1. Create base events and trigger refs
  (tickEvent, tickTriggerRef)   <- newEventWithTriggerRef
  (inputEvent, inputTriggerRef) <- newEventWithTriggerRef

  -- 2. Build the FRP network:
  --    Hold the latest InputState in a Behavior, and attach it to the tickEvent
  inputBehavior <- hold emptyInputState inputEvent
  let tickWithInputEvent = attach inputBehavior tickEvent

  -- 3. Subscribe to the combined event so we can read it in the host loop
  combinedHandle <- subscribeEvent tickWithInputEvent

  -- 4. Create trigger firing functions
  let fireTick dt = runSpiderHost $ do
        mTrigger <- liftIO $ readIORef tickTriggerRef
        case mTrigger of
          Just trigger -> fireEventsAndRead [trigger :=> Identity dt] $ do
            mRead <- readEvent combinedHandle
            case mRead of
              Just readVal -> do
                val <- readVal
                return (Just val)
              Nothing -> return Nothing
          Nothing      -> return Nothing

      fireInput input = runSpiderHost $ do
        mTrigger <- liftIO $ readIORef inputTriggerRef
        case mTrigger of
          Just trigger -> fireEvents [trigger :=> Identity input]
          Nothing      -> return ()

  return (GameTriggers fireTick fireInput, combinedHandle)
