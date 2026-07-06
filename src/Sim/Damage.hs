-- | Unified damage resolution, once per fixed sub-step, AFTER physics moved
--   everyone — collecting all hits before applying them avoids same-step
--   ordering bugs (docs/ARCHITECTURE.md §3.4).
--
--   Player attacks (hitboxes from "Sim.EnemyCore") hurt enemies; enemy
--   contact and projectiles hurt the player. Death is only ever an emitted
--   fact: enemies die with 'EvEnemyKilled', the player with 'EvPlayerDied'.
module Sim.Damage
  ( resolveDamage
  ) where

import Apecs
import Control.Monad (when, forM_)
import qualified Data.Map.Strict as M

import Core.Components
import Core.Config
import Core.Types
import Enemy.Script (EnemyDef(..))
import Sim.EnemyCore (playerHitbox, playerAttackDamage)
import Sim.ParticleCore (hitBurst)
import Sim.Particles (emitParticles)
import Sim.Rules (emitEvent, aabbOverlap)

-- | One damage pass.
resolveDamage :: M.Map EnemyId EnemyDef -> Double -> Game ()
resolveDamage defs dt = do
  -- Tick the timers first.
  cmap $ \(EnemyHurt t) -> EnemyHurt (max 0.0 (t - dt))
  cmap $ \(Player, Invuln t) -> Invuln (max 0.0 (t - dt))

  cmapM_ $ \(Player, Position ppos, Collider psize, cstate :: CombatState
            , Facing dir, playerEty) -> do
    StatsCache stats <- get playerEty

    -- 1. Player attack hitbox vs enemies.
    forM_ (playerHitbox cstate dir ppos psize) $ \(hbPos, hbSize) -> do
      let dmg = playerAttackDamage cstate (dsAtk stats)
      cmapM_ $ \(Enemy eid, Position epos, Collider esize
                , EnemyHp hp, EnemyHurt hurt, enemyEty) ->
        when (hurt <= 0.0 && aabbOverlap hbPos hbSize epos esize) $ do
          let hp' = hp - dmg
              center = epos + esize / 2.0
          emitParticles (hitBurst center (Just dir))
          if hp' <= 0.0
            then do
              destroy enemyEty
                (Proxy @( (Enemy, EnemyBrain, Position, Velocity)
                        , (Collider, Gravity, IsGrounded)
                        , (EnemyHp, EnemyHurt) ))
              emitEvent (EvEnemyKilled eid)
            else set enemyEty (EnemyHp hp', EnemyHurt enemyHurtCooldown)

    -- 2. Enemy bodies and projectiles vs the player (respecting i-frames).
    Invuln iframes <- get playerEty
    when (iframes <= 0.0) $ do
      contact <- cfold
        (\acc (Enemy eid, Position epos, Collider esize) ->
           if aabbOverlap ppos psize epos esize
             then maybe acc ((: acc) . edDamage) (M.lookup eid defs)
             else acc)
        []

      hits <- cfoldM
        (\acc (Projectile dmg, Position prPos, Collider prSize, prEty) ->
           if aabbOverlap ppos psize prPos prSize
             then do
               destroy prEty (Proxy @(Projectile, Position, Velocity, Collider))
               return (dmg : acc)
             else return acc)
        []

      let total = sum (take 1 contact) + sum hits
      when (total > 0.0) $ do
        vitals :: Vitals <- get playerEty
        let hp' = vHp vitals - total
        emitParticles (hitBurst (ppos + psize / 2.0) Nothing)
        set playerEty (Invuln playerInvulnDuration)
        if hp' <= 0.0
          then do
            set playerEty vitals { vHp = 0.0 }
            emitEvent EvPlayerDied
          else set playerEty vitals { vHp = hp' }
