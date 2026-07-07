module RegistrySpec (spec) where

import Data.List (isInfixOf)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

import Core.Types (ItemId(..), ItemCategory(..), EquipSlot(..))
import Items.Registry
import Script.Sexp (parseSexps)

spec :: Spec
spec = do
  describe "compileRegistry" $ do
    it "compiles a full item definition" $ do
      let Right reg = compile
            "(item potion (name \"POTION\") (category potion) (stack 9) \
            \ (color 220 40 40) (use (heal 30)) (desc \"HEALS\"))"
          Just def = lookupItem reg (ItemId "potion")
      defName def `shouldBe` "POTION"
      defCategory def `shouldBe` CatPotion
      defStack def `shouldBe` 9
      defColor def `shouldBe` (220, 40, 40)
      defUse def `shouldBe` [Heal 30]

    it "compiles equipment with slot and stats" $ do
      let Right reg = compile
            "(item sword (name \"SWORD\") (category equip weapon) (stats (atk 5) (spd 10)))"
          Just def = lookupItem reg (ItemId "sword")
      defCategory def `shouldBe` CatEquip SlotWeapon
      statAtk (defStats def) `shouldBe` 5
      statSpdPct (defStats def) `shouldBe` 10

    it "defaults: stack 1, no use, zero stats" $ do
      let Right reg = compile "(item rock (name \"ROCK\") (category general))"
          Just def = lookupItem reg (ItemId "rock")
      defStack def `shouldBe` 1
      defUse def `shouldBe` []
      defStats def `shouldBe` emptyStats

    it "rejects a missing name" $
      compile "(item x (category general))" `shouldSatisfy` failsWith "missing (name"

    it "rejects an unknown category" $
      compile "(item x (name \"X\") (category food))" `shouldSatisfy` failsWith "bad category"

    it "rejects an unknown equip slot" $
      compile "(item x (name \"X\") (category equip tail))" `shouldSatisfy` failsWith "unknown equip slot"

    it "rejects duplicate ids" $
      compile "(item x (name \"A\") (category general)) (item x (name \"B\") (category general))"
        `shouldSatisfy` failsWith "duplicate item id"

    it "rejects an unknown use effect" $
      compile "(item x (name \"X\") (category potion) (use (explode 5)))"
        `shouldSatisfy` failsWith "unknown use effect"

  describe "requireItem" $ do
    it "passes for known ids and fails with context for unknown ids" $ do
      let Right reg = compile "(item x (name \"X\") (category general))"
      requireItem reg "lvl" (ItemId "x") `shouldSatisfy` isRight
      requireItem reg "lvl" (ItemId "y") `shouldSatisfy` failsWith "lvl: unknown item id 'y'"
  where
    compile src = parseSexps (T.pack src) >>= compileRegistry M.empty
    failsWith needle r = case r of
      Left err -> needle `isInfixOf` err
      Right _  -> False
    isRight (Right _) = True
    isRight _         = False
