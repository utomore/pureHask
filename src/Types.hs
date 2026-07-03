module Types where

import Apecs
import Linear (V2(..))

-- | Position of an entity in 2D space (x, y).
newtype Position = Position (V2 Double) deriving (Eq, Show)
instance Component Position where
  type Storage Position = Map Position

-- | Velocity of an entity in 2D space (dx, dy) in pixels per second.
newtype Velocity = Velocity (V2 Double) deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Map Velocity

-- | Axis-Aligned Bounding Box (AABB) size: (width, height) in pixels.
newtype Collider = Collider (V2 Double) deriving (Eq, Show)
instance Component Collider where
  type Storage Collider = Map Collider

-- | Gravity acceleration applied to the entity (pixels/sec^2).
newtype Gravity = Gravity Double deriving (Eq, Show)
instance Component Gravity where
  type Storage Gravity = Map Gravity

-- | Indicates if the entity is currently touching the ground.
newtype IsGrounded = IsGrounded Bool deriving (Eq, Show)
instance Component IsGrounded where
  type Storage IsGrounded = Map IsGrounded

-- | Marker component for the player entity.
data Player = Player deriving (Eq, Show)
instance Component Player where
  type Storage Player = Unique Player

-- | Marker component for the goal/endpoint entity.
data Goal = Goal deriving (Eq, Show)
instance Component Goal where
  type Storage Goal = Unique Goal

-- Generate the World type and its associated storage instances.
makeWorld "World" [
  ''Position,
  ''Velocity,
  ''Collider,
  ''Gravity,
  ''IsGrounded,
  ''Player,
  ''Goal
  ]

-- | Type synonym for the game monad, wrapping our ECS System.
type Game a = System World a

--------------------------------------------------------------------------------
-- Game Constants
--------------------------------------------------------------------------------

-- | Tile size in pixels.
tileSize :: Double
tileSize = 32.0

-- | Gravity acceleration (pixels/sec^2).
gravityAccel :: Double
gravityAccel = 1500.0

-- | Maximum downward falling speed (pixels/sec).
terminalVelocity :: Double
terminalVelocity = 900.0

-- | Horizonal movement speed (pixels/sec).
playerSpeed :: Double
playerSpeed = 300.0

-- | Jump vertical speed (pixels/sec). Negative because y goes down in SDL.
jumpSpeed :: Double
jumpSpeed = -550.0
