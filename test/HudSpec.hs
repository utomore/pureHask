module HudSpec (spec) where

import Test.Hspec

import Hud.Machine

spec :: Spec
spec = do
  describe "toast lifecycle" $ do
    it "pushed toasts become visible" $ do
      let hud = pushToasts ["A", "B"] emptyHud
      visibleToasts hud `shouldBe` ["A", "B"]

    it "toasts expire after their lifetime" $ do
      let hud = pushToasts ["A"] emptyHud
          later = tickHud 3.0 hud
      visibleToasts later `shouldBe` []

    it "only three toasts are visible; the rest queue up" $ do
      let hud = pushToasts ["A", "B", "C", "D"] emptyHud
      visibleToasts hud `shouldBe` ["A", "B", "C"]

    it "queued toasts do not age until they are visible" $ do
      let hud = pushToasts ["A", "B", "C", "D"] emptyHud
          -- Age the first wave away entirely.
          later = tickHud 3.0 hud
      visibleToasts later `shouldBe` ["D"]
      -- D is still fresh: it only starts aging now.
      visibleToasts (tickHud 1.0 later) `shouldBe` ["D"]
