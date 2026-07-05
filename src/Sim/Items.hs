-- | Pure item-usage logic: what consuming an item does to the player, and
--   backpack bookkeeping. Apecs glue lives in "Sim.Rules".
module Sim.Items
  ( applyEffects
  , firstPotion
  , consumeOne
  , backpackRows
  ) where

import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Core.Types (ItemId, ItemCategory(..), Vitals(..))
import Items.Registry

-- | Apply a consumable's effects to the player's vitals (clamped to maxima).
applyEffects :: [UseEffect] -> Vitals -> Vitals
applyEffects effects v = foldl apply v effects
  where
    apply vt (Heal n)           = vt { vHp = min (vMaxHp vt) (vHp vt + n) }
    apply vt (RestoreMp n)      = vt { vMp = min (vMaxMp vt) (vMp vt + n) }
    apply vt (RestoreStamina n) = vt { vStamina = min (vMaxStamina vt) (vStamina vt + n) }

-- | The first potion (by id order) present in the backpack.
firstPotion :: ItemRegistry -> Map ItemId Int -> Maybe ItemDef
firstPotion registry backpack =
  case [ def
       | (iid, count) <- M.toAscList backpack
       , count > 0
       , Just def <- [lookupItem registry iid]
       , defCategory def == CatPotion
       ] of
    (d : _) -> Just d
    []      -> Nothing

-- | Remove one unit of an item; drops the key at zero.
consumeOne :: ItemId -> Map ItemId Int -> Map ItemId Int
consumeOne = M.update (\n -> if n <= 1 then Nothing else Just (n - 1))

-- | Backpack contents as menu rows, grouped by category (general, potion,
--   equipment, quest) and then by id.
backpackRows :: ItemRegistry -> Map ItemId Int -> [(ItemId, ItemCategory, Int)]
backpackRows registry backpack =
  sortOn (\(iid, cat, _) -> (rank cat, iid))
    [ (iid, defCategory def, count)
    | (iid, count) <- M.toAscList backpack
    , Just def <- [lookupItem registry iid]
    ]
  where
    rank :: ItemCategory -> Int
    rank CatGeneral   = 0
    rank CatPotion    = 1
    rank (CatEquip _) = 2
    rank CatQuest     = 3
