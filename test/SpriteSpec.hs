module SpriteSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Test.Hspec

import Core.Types (CombatState(..))
import Script.Sexp (parseSexps)
import Sprite.Core
import Sprite.Script

testDefs :: [SpriteDef]
testDefs =
  let Right defs = parseSexps (T.pack src) >>= compileSprites
  in defs
  where
    src = "(sprite player (sheet player.bmp) (frame-size 24 24)\
          \  (color-key 255 0 255)\
          \  (anim idle (row 0) (frames 2) (fps 4))\
          \  (anim run  (row 1) (frames 4) (fps 10)))"

player :: SpriteDef
player = head testDefs

-- | A minimal 96x48 bottom-up BMP header (what 'bmpDimensions' reads).
fakeBmp :: Int -> Int -> BS.ByteString
fakeBmp w h = BS.pack $ concat
  [ [0x42, 0x4D]                    -- magic "BM"
  , replicate 16 0                  -- sizes/offsets (unused here)
  , le32 w
  , le32 h
  ]
  where
    le32 n = map (\s -> fromIntegral ((n `div` (256 ^ s)) `mod` 256)) [0 .. 3 :: Int]

spec :: Spec
spec = do
  describe "Sprite.Script.compileSprites" $ do
    it "compiles sheet, grid and animations" $ do
      sdSheet player `shouldBe` "player.bmp"
      (sdFrameW player, sdFrameH player) `shouldBe` (24, 24)
      sdColorKey player `shouldBe` Just (255, 0, 255)
      map adName (sdAnims player) `shouldBe` ["idle", "run"]

    it "rejects a sprite without animations" $ do
      let r = parseSexps "(sprite s (sheet s.bmp) (frame-size 8 8))"
                >>= compileSprites
      r `shouldSatisfy` either (const True) (const False)

    it "rejects duplicate animation names" $ do
      let r = parseSexps
                "(sprite s (sheet s.bmp) (frame-size 8 8)\
                \ (anim idle) (anim idle))" >>= compileSprites
      r `shouldSatisfy` either (const True) (const False)

  describe "bmpDimensions" $ do
    it "reads the header of a bottom-up BMP" $
      bmpDimensions (fakeBmp 96 48) `shouldBe` Right (96, 48)

    it "rejects a non-BMP file" $
      bmpDimensions (BS.replicate 64 0)
        `shouldSatisfy` either (const True) (const False)

  describe "frame stepping" $ do
    let Just run = animNamed player "run"

    it "starts at frame 0" $
      frameIndex run 0.0 `shouldBe` 0

    it "advances at the configured fps and loops" $ do
      frameIndex run 0.10 `shouldBe` 1   -- 10 fps
      frameIndex run 0.35 `shouldBe` 3
      frameIndex run 0.45 `shouldBe` 0   -- wrapped

    it "locates the frame inside the sheet grid" $
      frameOrigin player run 2 `shouldBe` (48, 24)

  describe "animation preference" $ do
    it "falls through missing names to ones the sheet has" $ do
      -- the test sheet has no "dash" row: dashing falls back to run
      adName <$> animOrFallback player (playerAnimPrefs (StateDashing 0.1) True)
        `shouldBe` Just "run"

    it "picks idle when standing still" $
      adName <$> animOrFallback player (playerAnimPrefs (StateIdle 0.0) False)
        `shouldBe` Just "idle"

    it "movement prefs pick run only while moving" $ do
      adName <$> animOrFallback player (movementAnimPrefs True) `shouldBe` Just "run"
      adName <$> animOrFallback player (movementAnimPrefs False) `shouldBe` Just "idle"
