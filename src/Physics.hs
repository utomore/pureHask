module Physics where

import Apecs
import Linear (V2(..))
import Types
import Map

-- | Data structure representing the current key states from input.
data InputState = InputState
  { inputLeft  :: !Bool
  , inputRight :: !Bool
  , inputJump  :: !Bool
  } deriving (Eq, Show)

-- | Initial empty input state.
emptyInputState :: InputState
emptyInputState = InputState False False False

-- | Helper for 2D AABB bounding box intersection check.
aabbOverlap :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Bool
aabbOverlap (V2 x1 y1) (V2 w1 h1) (V2 x2 y2) (V2 w2 h2) =
  x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2

-- | Physics update loop: updates gravity, movement, and tilemap collisions.
stepPhysics :: Tilemap -> Double -> Game ()
stepPhysics tilemap dt = cmap $ \(Position pos, Velocity (V2 vx vy), Collider size, IsGrounded _, Gravity g) ->
  let
    -- Apply gravity to vertical speed (clamped to terminal velocity)
    vy' = min terminalVelocity (vy + g * dt)
    -- Resolve grid collisions
    (newPos, newVel, grounded) = resolveCollisions tilemap pos (V2 vx vy') size dt
  in (Position newPos, Velocity newVel, IsGrounded grounded)

-- | Controls the player entity based on keyboard input states.
controlPlayer :: InputState -> Game ()
controlPlayer input = cmap $ \(Player, Velocity (V2 _ vy), IsGrounded grounded) ->
  let
    -- Calculate horizontal velocity
    vx' = case (inputLeft input, inputRight input) of
            (True, False) -> -playerSpeed
            (False, True) -> playerSpeed
            _             -> 0.0
            
    -- Calculate jump velocity (only if grounded)
    vy' = if inputJump input && grounded
          then jumpSpeed
          else vy
          
    -- Clear grounded state if jumping
    grounded' = if inputJump input && grounded then False else grounded
  in (Velocity (V2 vx' vy'), IsGrounded grounded')

-- | Checks for death (falling out of map bounds) and win (reaching the goal).
--   Resets the player to spawn position if either condition is met.
checkWinLoss :: V2 Double -> V2 Double -> Double -> Game ()
checkWinLoss playerSpawn goalPos mapHeightLimit =
  cmapM_ $ \(Player, Position pos, Collider size, ety) -> do
    let V2 _ py = pos
        dead = py > mapHeightLimit
        
        -- Goal is size tileSize x tileSize
        win = aabbOverlap pos size goalPos (V2 tileSize tileSize)
        
    if dead || win
      then do
        -- Reset player position and velocity
        set ety (Position playerSpawn, Velocity (V2 0.0 0.0), IsGrounded False)
      else return ()
