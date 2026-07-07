-- | Entity instantiation: turning 'LevelData' into a populated ECS world and
--   resetting the player on respawn. The only module that knows which
--   components a freshly spawned entity carries.
module Sim.Spawn
  ( spawnLevel
  , respawnPlayer
  , PlayerPersist(..)
  , capturePlayer
  , applyPlayer
  ) where

import Apecs
import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import Linear (V2(..))

import Core.Components
import Core.Config
import Core.Types
import Sim.EquipCore (applyMaxima)
import World.Level

-- | Populate a FRESH world from level data. "Main" creates a brand-new world
--   per level (worlds are cheap); this function never needs to clean up.
spawnLevel :: LevelData -> Game ()
spawnLevel lvl = do
  set global (UIState False)
  set global (EventQueue [])

  ety <- newEntity ( Player
                   , Position (ldPlayerSpawn lvl)
                   , Velocity (V2 0.0 0.0)
                   , Collider (V2 playerSize playerSize)
                   , Gravity gravityAccel
                   )
  set ety ( IsGrounded False
          , Facing DirRight
          , StateIdle 0.0
          , Backpack M.empty
          )
  set ety ( DoubleJump False
          , PlayerHook HookRetracted
          , fullVitals playerMaxHp playerMaxMp playerMaxStamina
          , Invuln 0.0
          )
  set ety ( Equipped M.empty
          , StatsCache baseStats
          , Talents emptyTalentState
          )

  _ <- newEntity ( Goal
                 , Position (ldGoal lvl)
                 , Collider (V2 tileSize tileSize)
                 )

  forM_ (ldItems lvl) $ \(itemPos, item) -> do
    _ <- newEntity ( Position itemPos
                   , Collider (V2 16.0 16.0)
                   , Item item
                   )
    return ()

-- | Put the player back at the spawn point with a clean slate (items are kept;
--   dying does not reset collection progress). Vitals refill to the cached
--   maxima (talents may raise them) — respawning with 0 hp would be an
--   instant death loop.
respawnPlayer :: V2 Double -> Game ()
respawnPlayer spawnPos =
  cmapM_ $ \(Player, ety) -> do
    StatsCache stats <- get ety
    set ety ( Position spawnPos
            , Velocity (V2 0.0 0.0)
            , IsGrounded False
            , StateIdle 0.0
            )
    set ety ( PlayerHook HookRetracted
            , DoubleJump False
            , applyMaxima stats (fullVitals playerMaxHp playerMaxMp playerMaxStamina)
            , Invuln 0.0
            )

--------------------------------------------------------------------------------
-- Cross-level persistence
--------------------------------------------------------------------------------

-- | The player state that survives level switches and lives in save files.
--   Worlds are rebuilt per level; this record is what carries over.
data PlayerPersist = PlayerPersist
  { ppBackpack :: !(M.Map ItemId Int)
  , ppEquipped :: !(M.Map EquipSlot ItemId)
  , ppVitals   :: !Vitals
  , ppStats    :: !DerivedStats
  , ppTalents  :: !TalentState
  } deriving (Eq, Show)

-- | Read the persistent player state out of the current world.
capturePlayer :: Game (Maybe PlayerPersist)
capturePlayer = do
  results <- cfoldM
    (\acc (Player, Backpack bp, ety) -> do
        Equipped eq <- get ety
        vitals :: Vitals <- get ety
        StatsCache stats <- get ety
        Talents talents <- get ety
        return (PlayerPersist bp eq vitals stats talents : acc))
    []
  return $ case results of
    (p : _) -> Just p
    []      -> Nothing

-- | Write a persistent player state into a freshly spawned world.
applyPlayer :: PlayerPersist -> Game ()
applyPlayer pp =
  cmapM_ $ \(Player, ety) -> do
    set ety ( Backpack (ppBackpack pp)
            , Equipped (ppEquipped pp)
            , ppVitals pp
            , StatsCache (ppStats pp)
            )
    set ety (Talents (ppTalents pp))
