-- | Pure NPC brains: the movement state machine (patrol/idle with pauses and
--   chatter bubbles) and dialogue rule selection. Apecs glue is "Sim.Npc".
module Npc.Core
  ( NpcAi(..)
  , initialAi
  , stepNpcAi
  , evalDialogue
  ) where

import Data.Text (Text)
import Linear (V2(..))

import Core.Types (Direction(..))
import Npc.Script
import Script.Expr

-- | Per-NPC AI state.
data NpcAi = NpcAi
  { naDir        :: !Direction
  , naPauseLeft  :: !Double            -- ^ > 0 while paused at a patrol end
  , naChatTimer  :: !Double            -- ^ counts down to the next bubble
  , naChatIx     :: !Int               -- ^ which chatter line comes next
  , naBubble     :: !(Maybe (Text, Double))  -- ^ visible bubble + remaining life
  } deriving (Eq, Show)

-- | How long a chatter bubble stays visible.
bubbleLife :: Double
bubbleLife = 3.0

initialAi :: NpcDef -> NpcAi
initialAi def = NpcAi
  { naDir = DirRight
  , naPauseLeft = 0.0
  , naChatTimer = maybe 0.0 fst (ndChatter def)
  , naChatIx = 0
  , naBubble = Nothing
  }

-- | One sub-step of an NPC brain: returns the new state and the horizontal
--   velocity the body should move with.
stepNpcAi :: NpcDef -> Double -> V2 Double -> NpcAi -> (NpcAi, Double)
stepNpcAi def dt (V2 px _) ai0 =
  let ai1 = tickChatter (tickBubble ai0)
  in case ndMovement def of
       MoveIdle -> (ai1, 0.0)
       MovePatrol minX maxX speed pause
         | naPauseLeft ai1 > 0.0 ->
             (ai1 { naPauseLeft = naPauseLeft ai1 - dt }, 0.0)
         | otherwise ->
             case naDir ai1 of
               DirRight
                 | px >= maxX -> (ai1 { naDir = DirLeft, naPauseLeft = pause }, 0.0)
                 | otherwise  -> (ai1, speed)
               DirLeft
                 | px <= minX -> (ai1 { naDir = DirRight, naPauseLeft = pause }, 0.0)
                 | otherwise  -> (ai1, -speed)
  where
    tickBubble ai = case naBubble ai of
      Nothing -> ai
      Just (msg, life) ->
        let life' = life - dt
        in ai { naBubble = if life' <= 0.0 then Nothing else Just (msg, life') }

    tickChatter ai = case ndChatter def of
      Nothing -> ai
      Just (interval, lns) ->
        let t' = naChatTimer ai - dt
        in if t' <= 0.0
             then ai { naChatTimer = interval
                     , naChatIx = (naChatIx ai + 1) `mod` length lns
                     , naBubble = Just (lns !! naChatIx ai, bubbleLife)
                     }
             else ai { naChatTimer = t' }

-- | Pick the actions of the first dialogue rule whose condition holds
--   (rules are tried top to bottom; @default@ always holds).
evalDialogue :: ScriptEnv -> NpcDef -> [Action]
evalDialogue env def =
  case [ drActions r
       | r <- ndDialogue def
       , maybe True (evalCond env) (drCond r)
       ] of
    (acts : _) -> acts
    []         -> []
