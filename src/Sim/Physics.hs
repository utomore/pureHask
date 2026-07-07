-- | Physics stepping: gravity, hook flight, collision resolution, VFX aging
--   and projectile movement. Physics-driven combat transitions (plunge
--   landing, hook arrival) also live here because they depend on collision
--   results, not on input.
module Sim.Physics
  ( stepPhysics
  , tickVFX
  , updateProjectiles
  ) where

import Apecs
import Control.Monad (when)
import Linear (V2(..), norm)

import Core.Components
import Core.Config
import Core.Types
import Sim.ParticleCore (burstFor, landingBurst)
import Sim.Particles (emitParticles)
import World.Tilemap

-- | One fixed physics sub-step for all entities.
stepPhysics :: Tilemap -> Double -> Game ()
stepPhysics tilemap dt = do
  -- 1. Player physics (depends on CombatState).
  cmapM $ \( Player
           , Position pos
           , Velocity (V2 vx vy)
           , Collider size
           , IsGrounded grounded
           , cstate :: CombatState
           , ety :: Entity
           ) -> do
    PlayerHook hookState <- get ety

    -- Advance a flying hook; anchor it on solid contact, drop it past range.
    case hookState of
      HookFlying hPos hVel -> do
        let hPos' = hPos + hVel * pure dt
            playerCenter = pos + size / 2.0
            dist = norm (hPos' - playerCenter)
        if dist > maxHookDist
          then set ety (PlayerHook HookRetracted)
          else if anySolid tilemap hPos' (V2 8.0 8.0)
            then set ety (PlayerHook (HookAnchored hPos'))
            else set ety (PlayerHook (HookFlying hPos' hVel))
      _ -> return ()

    -- States that override gravity entirely.
    let applyGravity = case cstate of
          StateDashing _     -> False
          StateThrust _      -> False
          StateCharging _    -> False
          StatePlunge        -> False
          StateHookPulling _ -> False
          StateHookHanging _ -> False
          _                  -> True

        vy' = if applyGravity
              then min terminalVelocity (vy + gravityAccel * dt)
              else vy

    (newPos, newVel, grounded', cstate') <- case cstate of
      StateHookPulling anchor -> do
        let playerCenter = pos + size / 2.0
            toAnchor = anchor - playerCenter
            dist = norm toAnchor
        if dist < hookArriveDist
          then return (pos, V2 0.0 0.0, True, StateHookHanging anchor)
          else do
            let pullVel = (toAnchor / pure dist) * pure hookPullSpeed
                (np, nv, g') = resolveCollisions tilemap pos pullVel size dt
            return (np, nv, g', StateHookPulling anchor)

      StateHookHanging anchor ->
        return (pos, V2 0.0 0.0, True, StateHookHanging anchor)

      StatePlunge -> do
        let (np, nv, g') = resolveCollisions tilemap pos (V2 vx vy') size dt
        if g' && not grounded
          then do
            -- Impact: burst a shockwave at the player's feet.
            let V2 pw ph = size
                shockwavePos = np + V2 (pw / 2.0) (ph - 4.0)
            _ <- newEntity (Position shockwavePos, VFX 0.3 VFXShockwave Nothing)
            emitParticles (burstFor VFXShockwave shockwavePos Nothing)
            return (np, V2 0.0 0.0, True, StateIdle 0.0)
          else return (np, nv, g', StatePlunge)

      _ -> do
        let (np, nv, g') = resolveCollisions tilemap pos (V2 vx vy') size dt
        -- Landing dust; the speed threshold lives in the pure emitter.
        when (g' && not grounded) $ do
          let V2 pw ph = size
          emitParticles (landingBurst vy' (np + V2 (pw / 2.0) (ph - 2.0)))
        return (np, nv, g', cstate)

    -- Touching ground restores the double jump.
    when grounded' $ set ety (DoubleJump False)

    return (Position newPos, Velocity newVel, IsGrounded grounded', cstate')

  -- 2. Everything else with a body: plain gravity + collisions.
  cmap $ \( Position pos
          , Velocity (V2 vx vy)
          , Collider size
          , IsGrounded _
          , Gravity g
          , Not :: Not Player
          ) ->
    let vy' = min terminalVelocity (vy + g * dt)
        (newPos, newVel, grounded) = resolveCollisions tilemap pos (V2 vx vy') size dt
    in (Position newPos, Velocity newVel, IsGrounded grounded)

-- | Age VFX entities and destroy the expired ones.
tickVFX :: Double -> Game ()
tickVFX dt = cmapM_ $ \(VFX life vfxType mDir, ety) -> do
  let life' = life - dt
  if life' <= 0
    then destroy ety (Proxy @(Position, VFX))
    else set ety (VFX life' vfxType mDir)

-- | Move projectiles; destroy them on solid contact. (No live spawner yet —
--   kept for the planned ranged enemies; see docs/ARCHITECTURE.md.)
updateProjectiles :: Tilemap -> Double -> Game ()
updateProjectiles tilemap dt =
  cmapM_ $ \(Projectile _, Position pos, Velocity vel, Collider size, ety) -> do
    let pos' = pos + vel * pure dt
    if anySolid tilemap pos' size
      then destroy ety (Proxy @(Position, Velocity, Collider, Projectile))
      else set ety (Position pos')
