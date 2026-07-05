module ItemsSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

import Core.Types
import Items.Registry
import Script.Sexp (parseSexps)
import Sim.Items

testRegistry :: ItemRegistry
testRegistry =
  let Right reg = parseSexps (T.pack src) >>= compileRegistry
  in reg
  where
    src = "(item potion-a (name \"A\") (category potion) (use (heal 30)))\
          \(item potion-b (name \"B\") (category potion) (use (restore-stamina 40)))\
          \(item key (name \"K\") (category quest))"

vitals50 :: Vitals
vitals50 = Vitals { vHp = 50, vMaxHp = 100, vMp = 10, vMaxMp = 50
                  , vStamina = 20, vMaxStamina = 100 }

spec :: Spec
spec = do
  describe "applyEffects" $ do
    it "heals HP up to the cap" $ do
      vHp (applyEffects [Heal 30] vitals50) `shouldBe` 80
      vHp (applyEffects [Heal 999] vitals50) `shouldBe` 100

    it "applies multiple effects" $ do
      let v = applyEffects [Heal 10, RestoreMp 15, RestoreStamina 30] vitals50
      (vHp v, vMp v, vStamina v) `shouldBe` (60, 25, 50)

  describe "firstPotion" $ do
    it "finds the first potion by id order, skipping non-potions" $ do
      let bp = M.fromList [(ItemId "key", 1), (ItemId "potion-b", 2)]
      fmap defId (firstPotion testRegistry bp) `shouldBe` Just (ItemId "potion-b")

    it "returns Nothing when the backpack has no potions" $ do
      let bp = M.fromList [(ItemId "key", 1)]
      firstPotion testRegistry bp `shouldBe` Nothing

  describe "consumeOne" $ do
    it "decrements a stack" $
      consumeOne (ItemId "x") (M.fromList [(ItemId "x", 3)])
        `shouldBe` M.fromList [(ItemId "x", 2)]

    it "removes the entry at zero" $
      consumeOne (ItemId "x") (M.fromList [(ItemId "x", 1)])
        `shouldBe` M.empty
