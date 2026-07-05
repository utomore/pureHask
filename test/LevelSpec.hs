module LevelSpec (spec) where

import Linear (V2(..))
import Test.Hspec

import Core.Config (tileSize)
import Core.Types (ItemId(..))
import World.Level
import World.Tilemap

goodLevel :: String
goodLevel = unlines
  [ "#####"
  , "#P K#"
  , "#  G#"
  , "#####"
  ]

spec :: Spec
spec = do
  describe "parseLevel" $ do
    it "extracts the player spawn" $ do
      let Right lvl = parseLevel "t" goodLevel
      ldPlayerSpawn lvl `shouldBe` V2 tileSize tileSize

    it "extracts the goal" $ do
      let Right lvl = parseLevel "t" goodLevel
      ldGoal lvl `shouldBe` V2 (3.0 * tileSize) (2.0 * tileSize)

    it "extracts items with their id" $ do
      let Right lvl = parseLevel "t" goodLevel
      map snd (ldItems lvl) `shouldBe` [ItemId "gold-key"]

    it "marker tiles parse as empty ground" $ do
      let Right lvl = parseLevel "t" goodLevel
      isTileSolid (ldTilemap lvl) 1 1 `shouldBe` False  -- P
      isTileSolid (ldTilemap lvl) 3 2 `shouldBe` False  -- G

    it "pads jagged lines to the widest row" $ do
      let Right lvl = parseLevel "t" "###\n#P G\n####"
      mapWidth (ldTilemap lvl) `shouldBe` 4

    it "fails loudly when the player marker is missing" $
      parseLevel "t" "###\n# G\n###"
        `shouldSatisfy` \r -> case r of
          Left err -> "'P'" `elem` words err || err /= ""
          Right _  -> False

    it "fails loudly when the goal marker is missing" $
      parseLevel "t" "###\n#P \n###"
        `shouldSatisfy` \r -> case r of
          Left _  -> True
          Right _ -> False

    it "fails on an empty file" $
      parseLevel "t" "" `shouldSatisfy` \r -> case r of
        Left _  -> True
        Right _ -> False
