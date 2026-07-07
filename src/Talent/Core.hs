-- | Pure talent rules: buying ranks, refunding everything at a respec
--   shrine, the derived-stat bonus, and the kill-milestone maths that turns
--   'Core.Types.statKills' into talent points. Apecs glue lives in
--   "Sim.Rules"; menu wiring in "Flow.Machine".
module Talent.Core
  ( rankOf
  , spentPoints
  , canLearn
  , learnTalent
  , respecTalents
  , talentBonus
  , talentRows
  , killPointsBetween
  ) where

import qualified Data.Map.Strict as M

import Core.Config (talentKillsPerPoint)
import Core.Types
import Talent.Script

-- | Learned rank of a node (0 = not learned).
rankOf :: TalentState -> TalentId -> Int
rankOf ts tid = M.findWithDefault 0 tid (tsRanks ts)

-- | Points sunk into the tree (what a respec refunds).
spentPoints :: [TalentDef] -> TalentState -> Int
spentPoints defs ts =
  sum [ tdCost def * rankOf ts (tdId def) | def <- defs ]

defOf :: [TalentDef] -> TalentId -> Maybe TalentDef
defOf defs tid = case [ d | d <- defs, tdId d == tid ] of
  (d : _) -> Just d
  []      -> Nothing

-- | Can one more rank be bought right now? Checks the node exists, is not
--   maxed, the prerequisite rank is met, and enough points are unspent.
canLearn :: [TalentDef] -> TalentState -> TalentId -> Bool
canLearn defs ts tid = case defOf defs tid of
  Nothing -> False
  Just def ->
    rankOf ts tid < tdMaxRank def
      && tsPoints ts >= tdCost def
      && case tdRequires def of
           Nothing             -> True
           Just (parent, rank) -> rankOf ts parent >= rank

-- | Buy one rank. 'Nothing' when 'canLearn' does not hold.
learnTalent :: [TalentDef] -> TalentId -> TalentState -> Maybe TalentState
learnTalent defs tid ts
  | canLearn defs ts tid
  , Just def <- defOf defs tid = Just TalentState
      { tsPoints = tsPoints ts - tdCost def
      , tsRanks  = M.insertWith (+) tid 1 (tsRanks ts)
      }
  | otherwise = Nothing

-- | Refund every learned rank (the respec shrine's effect). Ranks in nodes
--   that no longer exist refund nothing — 'Save.Codec.sanitizeSave' already
--   refunds those explicitly on load.
respecTalents :: [TalentDef] -> TalentState -> TalentState
respecTalents defs ts = TalentState
  { tsPoints = tsPoints ts + spentPoints defs ts
  , tsRanks  = M.empty
  }

-- | Total stat bonus of the learned tree, on top of 'baseStats'. Feeds
--   "Sim.EquipCore" as the base that equipment stacks onto.
talentBonus :: [TalentDef] -> TalentState -> DerivedStats
talentBonus defs ts = foldl addDef baseStats defs
  where
    addDef acc def =
      let rank = rankOf ts (tdId def)
      in if rank <= 0 then acc else foldl (addEff rank) acc (tdEffects def)
    addEff rank acc eff = case eff of
      EffAtk n        -> acc { dsAtk = dsAtk acc + n * rank }
      EffDef n        -> acc { dsDef = dsDef acc + n * rank }
      EffSpdPct n     -> acc { dsSpeedMult =
                                 dsSpeedMult acc + fromIntegral (n * rank) / 100.0 }
      EffMaxHp n      -> acc { dsMaxHp = dsMaxHp acc + n * rank }
      EffMaxStamina n -> acc { dsMaxStamina = dsMaxStamina acc + n * rank }

-- | The talent page rows for 'Core.Types.MenuEnv', in definition order.
talentRows :: [TalentDef] -> TalentState -> [(TalentId, Int, Bool)]
talentRows defs ts =
  [ (tdId def, rankOf ts (tdId def), canLearn defs ts (tdId def)) | def <- defs ]

-- | Talent points earned by raising the kill counter from @old@ to @new@:
--   one point per 'talentKillsPerPoint' kills, milestones crossed once.
killPointsBetween :: Int -> Int -> Int
killPointsBetween old new =
  max 0 (new `div` talentKillsPerPoint - old `div` talentKillsPerPoint)
