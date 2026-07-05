{-# OPTIONS_GHC -Wno-orphans #-}
-- | Apecs component declarations and the generated World.
--
--   Design rule: data types live in "Core.Types" (pure, testable); THIS module
--   is the only place that binds them to Apecs storage. The orphan-instance
--   warning is disabled deliberately — centralising every Component instance
--   here keeps the pure modules free of an Apecs dependency while giving the
--   ECS one obvious registration point. When you add a component:
--
--     1. define the data type (here if ECS-only, in "Core.Types" if a pure
--        state machine needs it),
--     2. add its @instance Component@ here,
--     3. add it to the 'makeWorld' list below.
module Core.Components where

import Apecs
import qualified Data.Map.Strict as M
import Linear (V2(..))

import Core.Types
import Npc.Core (NpcAi)

-- | Position of an entity in 2D space (top-left corner, pixels).
newtype Position = Position (V2 Double) deriving (Eq, Show)
instance Component Position where
  type Storage Position = Map Position

-- | Velocity of an entity (pixels per second).
newtype Velocity = Velocity (V2 Double) deriving (Eq, Show)
instance Component Velocity where
  type Storage Velocity = Map Velocity

-- | Axis-aligned bounding box size (width, height) in pixels.
newtype Collider = Collider (V2 Double) deriving (Eq, Show)
instance Component Collider where
  type Storage Collider = Map Collider

-- | Gravity acceleration applied to the entity (pixels/sec^2).
newtype Gravity = Gravity Double deriving (Eq, Show)
instance Component Gravity where
  type Storage Gravity = Map Gravity

-- | Whether the entity currently touches the ground.
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

-- | Facing component tracking direction.
newtype Facing = Facing Direction deriving (Eq, Show)
instance Component Facing where
  type Storage Facing = Map Facing

instance Component CombatState where
  type Storage CombatState = Map CombatState

-- | Visual effect component: (remainingLife, type, optionalFacing).
data VFX = VFX !Double !VFXType !(Maybe Direction) deriving (Eq, Show)
instance Component VFX where
  type Storage VFX = Map VFX

-- | Projectile marker with travel direction. Reserved for ranged enemies.
data Projectile = Projectile { projDir :: !Direction } deriving (Eq, Show)
instance Component Projectile where
  type Storage Projectile = Map Projectile

-- | An item lying in the level. The id is validated against the item
--   registry at startup.
newtype Item = Item ItemId deriving (Eq, Show)
instance Component Item where
  type Storage Item = Map Item

-- | Items the player carries, with stack counts.
newtype Backpack = Backpack (M.Map ItemId Int) deriving (Eq, Show)
instance Component Backpack where
  type Storage Backpack = Map Backpack

-- | Whether the mid-air double jump has been spent.
newtype DoubleJump = DoubleJump Bool deriving (Eq, Show)
instance Component DoubleJump where
  type Storage DoubleJump = Map DoubleJump

-- | Grappling hook attached to the player.
newtype PlayerHook = PlayerHook HookState deriving (Eq, Show)
instance Component PlayerHook where
  type Storage PlayerHook = Map PlayerHook

instance Component Vitals where
  type Storage Vitals = Map Vitals

-- | Gear worn in the five slots.
newtype Equipped = Equipped (M.Map EquipSlot ItemId) deriving (Eq, Show)
instance Component Equipped where
  type Storage Equipped = Map Equipped

-- | Cached total of base + equipment stats; recomputed on equip change.
newtype StatsCache = StatsCache DerivedStats deriving (Eq, Show)
instance Component StatsCache where
  type Storage StatsCache = Map StatsCache

-- | Marker for NPC entities (which definition they instantiate).
newtype Npc = Npc NpcId deriving (Eq, Show)
instance Component Npc where
  type Storage Npc = Map Npc

-- | The NPC's pure AI state (see "Npc.Core").
newtype NpcBrain = NpcBrain NpcAi deriving (Eq, Show)
instance Component NpcBrain where
  type Storage NpcBrain = Map NpcBrain

-- | Global HUD state (currently: backpack overlay visibility).
newtype UIState = UIState { showBackpack :: Bool } deriving (Eq, Show)
instance Semigroup UIState where _ <> b = b
instance Monoid UIState where mempty = UIState False
instance Component UIState where
  type Storage UIState = Global UIState

-- | Global outbox of simulation facts, drained once per frame by "Main" and
--   fired into the Reflex flow layer. Use 'Sim.Rules.emitEvent' to append.
newtype EventQueue = EventQueue [GameEvent] deriving (Eq, Show)
instance Semigroup EventQueue where
  EventQueue a <> EventQueue b = EventQueue (a <> b)
instance Monoid EventQueue where mempty = EventQueue []
instance Component EventQueue where
  type Storage EventQueue = Global EventQueue

makeWorld "World"
  [ ''Position
  , ''Velocity
  , ''Collider
  , ''Gravity
  , ''IsGrounded
  , ''Player
  , ''Goal
  , ''Facing
  , ''CombatState
  , ''VFX
  , ''Projectile
  , ''Item
  , ''Backpack
  , ''DoubleJump
  , ''PlayerHook
  , ''Vitals
  , ''Equipped
  , ''StatsCache
  , ''Npc
  , ''NpcBrain
  , ''UIState
  , ''EventQueue
  ]

-- | The game monad: an Apecs System over our World.
type Game a = System World a
