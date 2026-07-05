module TilemapSpec (spec) where

import qualified Data.Vector as V
import Linear (V2(..))
import Test.Hspec

import Core.Config (tileSize)
import World.Tilemap

-- | A 6x4 map:
--
--   ......
--   ..#...
--   ......
--   ######
sampleMap :: Tilemap
sampleMap = Tilemap
  { mapWidth  = 6
  , mapHeight = 4
  , mapTiles  = V.fromList (map toTile (concat rows))
  }
  where
    rows =
      [ "      "
      , "  #   "
      , "      "
      , "######"
      ]
    toTile '#' = Solid
    toTile _   = Empty

spec :: Spec
spec = do
  describe "isTileSolid" $ do
    it "reports solid tiles" $ do
      isTileSolid sampleMap 2 1 `shouldBe` True
      isTileSolid sampleMap 0 3 `shouldBe` True

    it "reports empty tiles" $
      isTileSolid sampleMap 0 0 `shouldBe` False

    it "treats horizontal out-of-bounds as solid walls" $ do
      isTileSolid sampleMap (-1) 0 `shouldBe` True
      isTileSolid sampleMap 6 0 `shouldBe` True

    it "treats vertical out-of-bounds as empty (sky above, pit below)" $ do
      isTileSolid sampleMap 0 (-1) `shouldBe` False
      isTileSolid sampleMap 0 4 `shouldBe` False

  describe "getOverlappingTiles" $ do
    it "returns a single tile for a box inside one tile" $
      getOverlappingTiles sampleMap (V2 4.0 4.0) (V2 8.0 8.0)
        `shouldBe` [(0, 0)]

    it "returns four tiles for a box crossing a tile corner" $
      getOverlappingTiles sampleMap (V2 (tileSize - 4.0) (tileSize - 4.0)) (V2 8.0 8.0)
        `shouldMatchList` [(0, 0), (0, 1), (1, 0), (1, 1)]

  describe "resolveCollisions" $ do
    let size = V2 24.0 24.0

    it "lets a body fall freely in open space" $ do
      let (V2 _ y, V2 _ vy, grounded) =
            resolveCollisions sampleMap (V2 8.0 0.0) (V2 0.0 100.0) size 0.1
      y `shouldSatisfy` (> 0.0)
      vy `shouldBe` 100.0
      grounded `shouldBe` False

    it "lands on the floor: snaps flush, zeroes vy, sets grounded" $ do
      let startY = 3.0 * tileSize - 25.0  -- 1px above the floor row
          (V2 _ y, V2 _ vy, grounded) =
            resolveCollisions sampleMap (V2 8.0 startY) (V2 0.0 200.0) size 0.1
      grounded `shouldBe` True
      vy `shouldBe` 0.0
      y `shouldSatisfy` (\v -> abs (v - (3.0 * tileSize - 24.0)) < 0.01)

    it "stops horizontal movement at a wall" $ do
      -- Running right into the block at column 2, row 1.
      let startX = 2.0 * tileSize - 25.0
          (V2 x _, V2 vx _, _) =
            resolveCollisions sampleMap (V2 startX tileSize) (V2 400.0 0.0) size 0.1
      vx `shouldBe` 0.0
      x `shouldSatisfy` (\v -> v <= 2.0 * tileSize - 24.0)

    it "reports grounded while standing still on the floor" $ do
      let onFloor = 3.0 * tileSize - 24.001
          (_, _, grounded) =
            resolveCollisions sampleMap (V2 8.0 onFloor) (V2 0.0 0.0) size 0.1
      grounded `shouldBe` True
