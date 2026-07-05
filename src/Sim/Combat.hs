-- | Apecs glue for the combat machine: gather the player's components into a
--   'CombatIn', run the pure 'combatStep', write the results back. No
--   gameplay decisions are made in this module.
module Sim.Combat
  ( controlPlayer
  ) where

import Apecs

import Core.Components
import Core.Types
import Sim.CombatCore

-- | Run one combat sub-step for the player.
controlPlayer :: FrameInput -> Double -> Game ()
controlPlayer input dt =
  cmapM $ \( Player
           , Position pos
           , Velocity vel
           , IsGrounded grounded
           , Facing facing
           , cstate :: CombatState
           , ety :: Entity
           ) -> do
    DoubleJump doubleJumped <- get ety
    PlayerHook hookState    <- get ety
    Collider size           <- get ety
    vitals :: Vitals        <- get ety
    StatsCache stats        <- get ety

    let out = combatStep CombatIn
          { ciInput        = input
          , ciDt           = dt
          , ciState        = cstate
          , ciGrounded     = grounded
          , ciFacing       = facing
          , ciVel          = vel
          , ciDoubleJumped = doubleJumped
          , ciHook         = hookState
          , ciPos          = pos
          , ciSize         = size
          , ciStamina      = vStamina vitals
          , ciSpeedMult    = dsSpeedMult stats
          }

    mapM_ spawnVfx (coVfx out)
    set ety (DoubleJump (coDoubleJumped out), PlayerHook (coHook out))
    set ety vitals { vStamina = coStamina out }

    return ( Facing (coFacing out)
           , coState out
           , Velocity (coVel out)
           , IsGrounded (coGrounded out)
           )
  where
    spawnVfx (VfxRequest pos life ty mDir) = do
      _ <- newEntity (Position pos, VFX life ty mDir)
      return ()
