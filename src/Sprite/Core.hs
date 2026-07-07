-- | Pure sprite-animation logic: which animation an entity should play and
--   which frame of it is visible at a given clock time. The renderer glue
--   ("Render.Sprites") only blits what these functions pick.
module Sprite.Core
  ( animNamed
  , animOrFallback
  , frameIndex
  , frameOrigin
  , playerAnimPrefs
  , movementAnimPrefs
  ) where

import Data.Text (Text)

import Core.Types (CombatState(..))
import Sprite.Script

-- | Find an animation by name.
animNamed :: SpriteDef -> Text -> Maybe AnimDef
animNamed def name = case [ a | a <- sdAnims def, adName a == name ] of
  (a : _) -> Just a
  []      -> Nothing

-- | First animation in the preference list that exists, else the sprite's
--   first animation (compilation guarantees there is one). A sheet can thus
--   start with a single @idle@ row and grow richer over time.
animOrFallback :: SpriteDef -> [Text] -> Maybe AnimDef
animOrFallback def prefs =
  case [ a | n <- prefs, Just a <- [animNamed def n] ] of
    (a : _) -> Just a
    []      -> case sdAnims def of
      (a : _) -> Just a
      []      -> Nothing

-- | The visible frame at clock time @t@ (seconds); animations loop.
frameIndex :: AnimDef -> Double -> Int
frameIndex anim t =
  max 0 (floor (t * adFps anim)) `mod` max 1 (adFrames anim)

-- | Top-left pixel of a frame inside the sheet.
frameOrigin :: SpriteDef -> AnimDef -> Int -> (Int, Int)
frameOrigin def anim frame =
  (frame * sdFrameW def, adRow anim * sdFrameH def)

-- | Animation preference for the player, from combat state and horizontal
--   movement. Names not present in the sheet fall through to the next.
playerAnimPrefs :: CombatState -> Bool -> [Text]
playerAnimPrefs st moving = case st of
  StateDashing _     -> ["dash", "run", "idle"]
  StateDashJump      -> ["dash", "jump", "run", "idle"]
  StateMelee _       -> ["attack", "idle"]
  StateCharging _    -> ["charge", "idle"]
  StateThrust _      -> ["thrust", "attack", "idle"]
  StatePlunge        -> ["plunge", "jump", "idle"]
  StateHookPulling _ -> ["hook", "jump", "idle"]
  StateHookHanging _ -> ["hook", "idle"]
  StateIdle _
    | moving    -> ["run", "walk", "idle"]
    | otherwise -> ["idle"]

-- | Animation preference for NPCs and enemies, from movement only.
movementAnimPrefs :: Bool -> [Text]
movementAnimPrefs moving
  | moving    = ["run", "walk", "idle"]
  | otherwise = ["idle"]
