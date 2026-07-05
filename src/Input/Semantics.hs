-- | The input-semantics state machine: raw key snapshots in, player 'Intent's
--   out. This is where edge detection and double-tap recognition live —
--   simulation code never compares "this frame vs last frame" key states.
--
--   The machine is a pure step function; "FRP.Network" folds it over the
--   per-frame event stream, and the test suite drives it directly.
module Input.Semantics
  ( IntentTracker(..)
  , initialTracker
  , stepIntents
  ) where

import Core.Config (doubleTapWindow)
import Core.Types

-- | Internal state of the intent machine: the previous raw snapshot (for
--   edge detection) and the remaining double-tap windows per direction.
data IntentTracker = IntentTracker
  { trkPrev       :: !RawInput
  , trkLeftTimer  :: !Double
  , trkRightTimer :: !Double
  } deriving (Eq, Show)

initialTracker :: IntentTracker
initialTracker = IntentTracker emptyRawInput 0.0 0.0

-- | Advance the machine by one frame. Returns the new state, the intents
--   fired this frame, and the currently held keys.
stepIntents :: Double -> RawInput -> IntentTracker -> (IntentTracker, FrameInput)
stepIntents dt raw trk = (tracker', FrameInput intents held)
  where
    prev = trkPrev trk

    pressed  f = f raw && not (f prev)
    released f = not (f raw) && f prev

    -- Tick down double-tap windows.
    leftT  = max 0.0 (trkLeftTimer trk - dt)
    rightT = max 0.0 (trkRightTimer trk - dt)

    -- A direction press either completes a double tap (dash) or opens a
    -- fresh window for that direction.
    (leftT', rightT', dashIntent)
      | pressed rawLeft =
          if leftT > 0.0
            then (0.0, rightT, [IntentDash DirLeft])
            else (doubleTapWindow, rightT, [])
      | pressed rawRight =
          if rightT > 0.0
            then (leftT, 0.0, [IntentDash DirRight])
            else (leftT, doubleTapWindow, [])
      | otherwise = (leftT, rightT, [])

    intents = concat
      [ dashIntent
      , [ IntentJump           | pressed rawJump ]
      , [ IntentAttackPress    | pressed rawAttack ]
      , [ IntentAttackRelease  | released rawAttack ]
      , [ IntentPickUp         | pressed rawPick ]
      , [ IntentToggleBackpack | pressed rawBackpack ]
      , [ IntentHook           | pressed rawHook ]
      , [ IntentUsePotion      | pressed rawUsePotion ]
      , [ IntentMenu           | pressed rawMenu ]
      , [ IntentNavLeft        | pressed rawLeft ]
      , [ IntentNavRight       | pressed rawRight ]
      , [ IntentNavUp          | pressed rawUp ]
      , [ IntentNavDown        | pressed rawDown ]
      ]

    held = HeldKeys
      { heldLeft   = rawLeft raw
      , heldRight  = rawRight raw
      , heldJump   = rawJump raw
      , heldAttack = rawAttack raw
      }

    tracker' = IntentTracker
      { trkPrev       = raw
      , trkLeftTimer  = leftT'
      , trkRightTimer = rightT'
      }
