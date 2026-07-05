-- | The HUD toast queue: short-lived messages (quest updates, pickups).
--   Pure machine, folded inside the Reflex network via "Game.Logic".
module Hud.Machine
  ( HudState(..)
  , emptyHud
  , tickHud
  , pushToasts
  , visibleToasts
  ) where

import Data.Text (Text)

-- | Seconds a toast stays on screen.
toastDuration :: Double
toastDuration = 2.8

-- | How many toasts are visible at once (older ones queue up behind).
maxVisibleToasts :: Int
maxVisibleToasts = 3

newtype HudState = HudState
  { hsToasts :: [(Text, Double)]  -- ^ message, remaining life (head = oldest)
  } deriving (Eq, Show)

emptyHud :: HudState
emptyHud = HudState []

-- | Age the visible toasts; queued ones (beyond the visible window) wait.
tickHud :: Double -> HudState -> HudState
tickHud dt (HudState toasts) =
  let (visible, queued) = splitAt maxVisibleToasts toasts
      aged = [ (t, life - dt) | (t, life) <- visible, life - dt > 0.0 ]
  in HudState (aged <> queued)

-- | Enqueue new toasts (they appear after the current ones).
pushToasts :: [Text] -> HudState -> HudState
pushToasts msgs (HudState toasts) =
  HudState (toasts <> [ (m, toastDuration) | m <- msgs ])

-- | The toasts to draw this frame, oldest first.
visibleToasts :: HudState -> [Text]
visibleToasts (HudState toasts) = map fst (take maxVisibleToasts toasts)
