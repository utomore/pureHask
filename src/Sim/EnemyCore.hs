-- | The enemy behavior-tree evaluator and the player-attack hitbox table,
--   both pure. "Sim.Enemy" is the Apecs glue that feeds positions in and
--   writes velocities/projectiles out; "Sim.Damage" resolves the hits.
--
--   Evaluation model (REACTIVE behavior tree):
--
--     * The whole tree is re-evaluated every fixed sub-step — nodes have no
--       memory of their own; whatever state a behaviour needs lives in the
--       'EnemyAi' blackboard (patrol direction/pause, attack cooldown).
--     * Conditions succeed or fail; actions always succeed after writing
--       their outputs. @select@ takes the first succeeding child, @sequence@
--       needs all children — together they read as prioritised rules.
--
--   This buys most of what a full BT gives (priorities, composability,
--   data-driven variety) with none of the per-node bookkeeping; if node
--   memory is ever needed (timers, one-shots), extend the blackboard, not
--   the evaluator.
module Sim.EnemyCore
  ( EnemyAi(..)
  , EnemyStep(..)
  , initialEnemyAi
  , stepEnemyAi
  , playerHitbox
  , playerAttackDamage
  ) where

import Linear (V2(..), norm)

import Core.Config
import Core.Types
import Enemy.Script

-- | The blackboard: everything tree actions may read or update across
--   sub-steps.
data EnemyAi = EnemyAi
  { eaHome        :: !(V2 Double)  -- ^ spawn point; patrol is centred on it
  , eaPatrolDir   :: !Direction
  , eaPatrolPause :: !Double       -- ^ remaining turn pause at a patrol edge
  , eaCooldown    :: !Double       -- ^ remaining shoot cooldown
  } deriving (Eq, Show)

initialEnemyAi :: V2 Double -> EnemyAi
initialEnemyAi home = EnemyAi home DirRight 0.0 0.0

-- | What one AI sub-step decided.
data EnemyStep = EnemyStep
  { esAi    :: !EnemyAi
  , esVx    :: !Double
  , esShoot :: !(Maybe (V2 Double, Double))
    -- ^ (unit direction towards the player, projectile speed)
  } deriving (Eq, Show)

-- | One fixed sub-step of an enemy brain: tick the blackboard timers, then
--   evaluate the behavior tree against the world snapshot.
stepEnemyAi :: EnemyDef -> Double -> Double -> V2 Double -> V2 Double
            -> EnemyAi -> EnemyStep
stepEnemyAi def dt hpFrac self player ai0 = snd (tick (edTree def) start)
  where
    ticked = ai0 { eaCooldown = max 0.0 (eaCooldown ai0 - dt) }
    start = EnemyStep ticked 0.0 Nothing

    toPlayer = player - self
    dist = norm toPlayer
    V2 dx _ = toPlayer
    towards = if dx < 0.0 then -1.0 else 1.0

    -- (succeeded?, outputs). Failed subtrees leave the step untouched.
    tick :: BTNode -> EnemyStep -> (Bool, EnemyStep)
    tick node bb = case node of
      BTSelect kids   -> selectFirst kids
      BTSequence kids -> allInOrder kids bb
      BTCond c        -> (condHolds c, bb)
      BTAct a         -> (True, act a bb)
      where
        selectFirst [] = (False, bb)
        selectFirst (k : rest) = case tick k bb of
          (True, bb') -> (True, bb')
          (False, _)  -> selectFirst rest

        allInOrder [] acc = (True, acc)
        allInOrder (k : rest) acc = case tick k acc of
          (True, acc') -> allInOrder rest acc'
          (False, _)   -> (False, bb)

    condHolds c = case c of
      CondPlayerWithin d -> dist <= d
      CondPlayerBeyond d -> dist > d
      CondHpBelow f      -> hpFrac < f
      CondCooldownReady  -> eaCooldown ticked <= 0.0

    act a bb = case a of
      ActChase speed -> bb { esVx = towards * speed }
      ActFlee speed  -> bb { esVx = negate towards * speed }
      ActStop        -> bb { esVx = 0.0 }
      ActShoot projSpeed cooldown
        | eaCooldown (esAi bb) <= 0.0 ->
            bb { esShoot = Just (toPlayer / pure (max 1.0 dist), projSpeed)
               , esAi = (esAi bb) { eaCooldown = cooldown }
               }
        | otherwise -> bb
      ActPatrol radius speed ->
        let ai = esAi bb
            V2 sx _ = self
            V2 hx _ = eaHome ai
            pastEdge = case eaPatrolDir ai of
              DirLeft  -> sx <= hx - radius
              DirRight -> sx >= hx + radius
        in if eaPatrolPause ai > 0.0
             then bb { esVx = 0.0
                     , esAi = ai { eaPatrolPause = eaPatrolPause ai - dt } }
             else if pastEdge
               then bb { esVx = 0.0
                       , esAi = ai { eaPatrolDir = flipDir (eaPatrolDir ai)
                                   , eaPatrolPause = patrolTurnPause } }
               else bb { esVx = signed (eaPatrolDir ai) speed }

    patrolTurnPause = 0.8
    flipDir DirLeft  = DirRight
    flipDir DirRight = DirLeft
    signed d v = if d == DirLeft then -v else v

--------------------------------------------------------------------------------
-- Player attacks
--------------------------------------------------------------------------------

-- | The active hitbox (top-left, size) of the player's current attack, if
--   any. Melee reuses the swipe geometry drawn by "Render.Draw"; plunge
--   hits directly below the diving body.
playerHitbox :: CombatState -> Direction -> V2 Double -> V2 Double
             -> Maybe (V2 Double, V2 Double)
playerHitbox cstate dir pos size = case cstate of
  StateMelee _ ->
    let off = if dir == DirLeft then V2 (-26.0) 0.0 else V2 18.0 0.0
    in Just (pos + off, V2 32.0 24.0)
  StateThrust _ ->
    let off = if dir == DirLeft then V2 (-36.0) 4.0 else V2 12.0 4.0
    in Just (pos + off, V2 48.0 16.0)
  StatePlunge ->
    let V2 pw ph = size
    in Just (pos + V2 ((pw - 20.0) / 2.0) (ph - 8.0), V2 20.0 24.0)
  _ -> Nothing

-- | Damage of the player's current attack: base + weapon atk, scaled per
--   move.
playerAttackDamage :: CombatState -> Int -> Double
playerAttackDamage cstate atkStat =
  let base = playerBaseAtk + fromIntegral atkStat
  in case cstate of
       StateThrust _ -> base * thrustDamageMult
       StatePlunge   -> base * plungeDamageMult
       _             -> base
