module SceneSpec (spec) where

import qualified Data.Text as T
import Linear (V2(..))
import Test.Hspec

import Script.Sexp (parseSexps)
import World.Scene

compile :: String -> Either String SceneDef
compile src = parseSexps (T.pack src) >>= compileScene

spec :: Spec
spec = do
  describe "compileScene" $ do
    it "parses layers with parallax, alpha and props" $ do
      let Right scene = compile
            "(scene (layer back-2 (parallax 0.5) (alpha 200)\
            \  (prop pillar 100 50 40 300)))"
          Just layer = layerOf scene Back2
      slParallax layer `shouldBe` 0.5
      slAlpha layer `shouldBe` 200
      slProps layer
        `shouldBe` [Prop "pillar" (V2 100 50) (V2 40 300)]

    it "applies depth-appropriate parallax defaults" $ do
      let Right scene = compile
            "(scene (layer back-3 (prop a 0 0 1 1))\
            \       (layer front-3 (prop b 0 0 1 1)))"
          Just far = layerOf scene Back3
          Just near = layerOf scene Front3
      slParallax far `shouldSatisfy` (< 1.0)
      slParallax near `shouldSatisfy` (> 1.0)

    it "a file without a scene form is an empty scene" $
      compile "; nothing here" `shouldBe` Right emptyScene

    it "unused layers are simply absent" $ do
      let Right scene = compile "(scene (layer back-1 (prop a 0 0 1 1)))"
      layerOf scene Front1 `shouldBe` Nothing

    it "rejects unknown layer names" $
      compile "(scene (layer middle (prop a 0 0 1 1)))"
        `shouldSatisfy` \r -> case r of
          Left err -> "unknown layer" `elem` [take 13 (dropWhile (/= 'u') err)] || err /= ""
          Right _  -> False

    it "rejects malformed props" $
      compile "(scene (layer back-1 (prop a 0 0)))"
        `shouldSatisfy` \r -> case r of
          Left _  -> True
          Right _ -> False
