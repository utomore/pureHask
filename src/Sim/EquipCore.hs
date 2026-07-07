-- | Pure equipment rules: putting gear on and off, and computing the derived
--   stat totals. Apecs glue lives in "Sim.Rules" ('Sim.Rules.equipById',
--   'Sim.Rules.unequipBySlot').
module Sim.EquipCore
  ( equipItem
  , unequipSlot
  , computeStats
  , applyMaxima
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Core.Config (playerMaxHp, playerMaxStamina)
import Core.Types (ItemId, EquipSlot, ItemCategory(..), DerivedStats(..),
                   Vitals(..))
import Items.Registry
import Sim.Items (consumeOne)

-- | Equip an item from the backpack. Fails (Nothing) when the item is not
--   equipment or not in the backpack. A previously equipped item in the same
--   slot goes back to the backpack.
equipItem :: ItemDef
          -> Map EquipSlot ItemId  -- ^ currently equipped
          -> Map ItemId Int        -- ^ backpack
          -> Maybe (Map EquipSlot ItemId, Map ItemId Int)
equipItem def equipped backpack = case defCategory def of
  CatEquip slot
    | M.findWithDefault 0 (defId def) backpack > 0 ->
        let removed = consumeOne (defId def) backpack
            returned = case M.lookup slot equipped of
              Just old -> M.insertWith (+) old 1 removed
              Nothing  -> removed
        in Just (M.insert slot (defId def) equipped, returned)
  _ -> Nothing

-- | Take the item in a slot off, back into the backpack.
unequipSlot :: EquipSlot
            -> Map EquipSlot ItemId
            -> Map ItemId Int
            -> (Map EquipSlot ItemId, Map ItemId Int)
unequipSlot slot equipped backpack = case M.lookup slot equipped of
  Nothing  -> (equipped, backpack)
  Just iid -> (M.delete slot equipped, M.insertWith (+) iid 1 backpack)

-- | Total derived stats for a set of equipped items, folded onto a starting
--   block — pass 'Core.Types.baseStats' for a bare player or
--   'Talent.Core.talentBonus' to stack gear on top of talents.
computeStats :: ItemRegistry -> DerivedStats -> Map EquipSlot ItemId -> DerivedStats
computeStats registry start equipped =
  foldl add start
    [ defStats def | iid <- M.elems equipped, Just def <- [lookupItem registry iid] ]
  where
    add ds st = ds
      { dsAtk       = dsAtk ds + statAtk st
      , dsDef       = dsDef ds + statDef st
      , dsSpeedMult = dsSpeedMult ds + fromIntegral (statSpdPct st) / 100.0
      }

-- | Bring the vitals' maxima in line with a stat block (talent max-HP /
--   max-stamina bonuses), clamping current values into the new range.
applyMaxima :: DerivedStats -> Vitals -> Vitals
applyMaxima ds v =
  let maxHp = playerMaxHp + fromIntegral (dsMaxHp ds)
      maxSt = playerMaxStamina + fromIntegral (dsMaxStamina ds)
  in v { vMaxHp = maxHp, vHp = min (vHp v) maxHp
       , vMaxStamina = maxSt, vStamina = min (vStamina v) maxSt
       }
