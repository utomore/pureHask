module EquipCoreSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

import Core.Types
import Items.Registry
import Script.Sexp (parseSexps)
import Sim.EquipCore

testRegistry :: ItemRegistry
testRegistry =
  let Right reg = parseSexps (T.pack src) >>= compileRegistry M.empty
  in reg
  where
    src = "(item sword-a (name \"SWORD A\") (category equip weapon) (stats (atk 5)))\
          \(item sword-b (name \"SWORD B\") (category equip weapon) (stats (atk 9)))\
          \(item boots (name \"BOOTS\") (category equip shoes) (stats (spd 15) (def 1)))\
          \(item potion (name \"POTION\") (category potion) (use (heal 30)))"

def' :: String -> ItemDef
def' name =
  let Just d = lookupItem testRegistry (ItemId (T.pack name)) in d

spec :: Spec
spec = do
  describe "equipItem" $ do
    it "equips gear from the backpack" $ do
      let bp = M.fromList [(ItemId "sword-a", 1)]
          Just (eq, bp') = equipItem (def' "sword-a") M.empty bp
      M.lookup SlotWeapon eq `shouldBe` Just (ItemId "sword-a")
      M.lookup (ItemId "sword-a") bp' `shouldBe` Nothing

    it "swapping returns the previous gear to the backpack" $ do
      let bp = M.fromList [(ItemId "sword-b", 1)]
          eq0 = M.fromList [(SlotWeapon, ItemId "sword-a")]
          Just (eq, bp') = equipItem (def' "sword-b") eq0 bp
      M.lookup SlotWeapon eq `shouldBe` Just (ItemId "sword-b")
      M.lookup (ItemId "sword-a") bp' `shouldBe` Just 1

    it "cannot equip a potion" $
      equipItem (def' "potion") M.empty (M.fromList [(ItemId "potion", 1)])
        `shouldBe` Nothing

    it "cannot equip gear that is not in the backpack" $
      equipItem (def' "sword-a") M.empty M.empty `shouldBe` Nothing

  describe "unequipSlot" $ do
    it "moves the gear back to the backpack" $ do
      let eq0 = M.fromList [(SlotWeapon, ItemId "sword-a")]
          (eq, bp) = unequipSlot SlotWeapon eq0 M.empty
      eq `shouldBe` M.empty
      M.lookup (ItemId "sword-a") bp `shouldBe` Just 1

    it "is a no-op for an empty slot" $
      unequipSlot SlotHead M.empty M.empty `shouldBe` (M.empty, M.empty)

  describe "computeStats" $ do
    it "starts from the given base line" $
      computeStats testRegistry baseStats M.empty `shouldBe` baseStats

    it "sums stats over all equipped gear" $ do
      let eq = M.fromList [ (SlotWeapon, ItemId "sword-b")
                          , (SlotShoes, ItemId "boots")
                          ]
          ds = computeStats testRegistry baseStats eq
      dsAtk ds `shouldBe` 9
      dsDef ds `shouldBe` 1
      dsSpeedMult ds `shouldBe` 1.15

    it "stacks gear on top of a talent bonus base" $ do
      let talentBase = baseStats { dsAtk = 4, dsMaxHp = 25 }
          eq = M.fromList [(SlotWeapon, ItemId "sword-a")]
          ds = computeStats testRegistry talentBase eq
      dsAtk ds `shouldBe` 9
      dsMaxHp ds `shouldBe` 25

  describe "applyMaxima" $ do
    it "raises the maxima by the stat bonuses" $ do
      let v = applyMaxima baseStats { dsMaxHp = 50, dsMaxStamina = 20 }
                (fullVitals 100 50 100)
      vMaxHp v `shouldBe` 150
      vMaxStamina v `shouldBe` 120

    it "clamps current values when the maxima shrink" $ do
      let v0 = (fullVitals 100 50 100) { vHp = 140, vMaxHp = 150 }
          v = applyMaxima baseStats v0
      vMaxHp v `shouldBe` 100
      vHp v `shouldBe` 100
