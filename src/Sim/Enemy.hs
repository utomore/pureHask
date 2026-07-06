-- | Apecs glue for enemies: spawning per level, stepping the pure brains
--   ("Sim.EnemyCore") and firing their projectiles. Damage resolution lives
--   in "Sim.Damage". No decisions here.
module Sim.Enemy
  ( spawnEnemiesForLevel
  , stepEnemies
  ) where

import Apecs
import Control.Monad (forM_)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Linear (V2(..))

import Core.Components
import Core.Config
import Core.Types
import Enemy.Script
import Sim.EnemyCore

-- | Spawn every instance of every enemy kind that belongs in the level.
spawnEnemiesForLevel :: M.Map EnemyId EnemyDef -> Text -> Game ()
spawnEnemiesForLevel defs levelName =
  forM_ (M.elems defs) $ \def ->
    forM_ [ p | (lvl, p) <- edSpawns def, lvl == levelName ] $ \pos -> do
      _ <- newEntity ( Enemy (edId def)
                     , EnemyBrain (initialEnemyAi pos)
                     , Position pos
                     , Velocity (V2 0.0 0.0)
                     , Collider (edSize def)
                     , Gravity gravityAccel
                     , IsGrounded False
                     , (EnemyHp (edHp def), EnemyHurt 0.0)
                     )
      return ()

-- | One sub-step of every enemy brain: the behavior tree decides the
--   horizontal velocity and possibly a shot; the shared physics moves the
--   body.
stepEnemies :: M.Map EnemyId EnemyDef -> Double -> Game ()
stepEnemies defs dt = do
  players <- cfold
    (\acc (Player, Position p, Collider s) -> (p + s / 2.0) : acc) []
  case players of
    [] -> return ()
    (playerCenter : _) ->
      cmapM $ \(Enemy eid, EnemyBrain ai, Position pos, Collider size
               , EnemyHp hp, Velocity (V2 _ vy)) ->
        case M.lookup eid defs of
          Nothing -> return (EnemyBrain ai, Velocity (V2 0.0 vy))
          Just def -> do
            let center = pos + size / 2.0
                hpFrac = hp / max 1.0 (edHp def)
                step = stepEnemyAi def dt hpFrac center playerCenter ai
            forM_ (esShoot step) $ \(dirUnit, speed) -> do
              _ <- newEntity
                ( Projectile (edDamage def)
                , Position (center - pure (enemyProjectileSize / 2.0))
                , Velocity (dirUnit * pure speed)
                , Collider (pure enemyProjectileSize)
                )
              return ()
            return (EnemyBrain (esAi step), Velocity (V2 (esVx step) vy))
