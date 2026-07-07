module AudioSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

import Audio.Script
import Core.Types
import Script.Sexp (parseSexps)

testDefs :: AudioDefs
testDefs =
  let Right defs = parseSexps (T.pack src) >>= compileAudio
  in defs
  where
    src = "(sfx pickup (file sfx/pickup.wav) (volume 96))\
          \(sfx death (file sfx/death.wav))\
          \(music title-theme (file music/title.wav) (volume 56))\
          \(music overworld (file music/overworld.wav))\
          \(music cave (file music/cave.wav))\
          \(on-event item-picked pickup)\
          \(on-event player-died death)\
          \(music-for-mode title title-theme)\
          \(music-for-mode playing overworld)\
          \(music-for-level 03-hollow cave)"

compileErr :: String -> Bool
compileErr src = either (const True) (const False)
  (parseSexps (T.pack src) >>= compileAudio)

spec :: Spec
spec = do
  describe "compileAudio" $ do
    it "compiles sounds, tracks and bindings" $ do
      M.size (adSfx testDefs) `shouldBe` 2
      M.size (adMusic testDefs) `shouldBe` 3
      M.lookup "item-picked" (adEventSfx testDefs) `shouldBe` Just "pickup"
      sfVolume (adSfx testDefs M.! "death") `shouldBe` 96  -- default

    it "rejects a binding to an unknown sfx id" $
      compileErr "(on-event item-picked nope)" `shouldBe` True

    it "rejects an unknown event name" $
      compileErr "(sfx s (file f.wav)) (on-event item-eaten s)" `shouldBe` True

    it "rejects an unknown mode name" $
      compileErr "(music m (file f.wav)) (music-for-mode victory m)"
        `shouldBe` True

    it "rejects a duplicate sfx id" $
      compileErr "(sfx s (file a.wav)) (sfx s (file b.wav))" `shouldBe` True

    it "rejects an out-of-range volume" $
      compileErr "(sfx s (file a.wav) (volume 200))" `shouldBe` True

  describe "sfxForEvent" $ do
    it "maps bound events to their sound" $ do
      sfxForEvent testDefs (EvItemPicked (ItemId "gold-key"))
        `shouldBe` Just "pickup"
      sfxForEvent testDefs EvPlayerDied `shouldBe` Just "death"

    it "unbound events are silent" $
      sfxForEvent testDefs EvGoalReached `shouldBe` Nothing

    it "a restored run is never a sound trigger" $
      sfxForEvent testDefs (EvRunRestored 0 emptyRunStats emptyQuestLog)
        `shouldBe` Nothing

  describe "musicFor" $ do
    it "plays the title theme on the title screen" $
      musicFor testDefs ModeTitle "01-training" `shouldBe` Just "title-theme"

    it "a level binding beats the generic playing track" $ do
      musicFor testDefs ModePlaying "03-hollow" `shouldBe` Just "cave"
      musicFor testDefs ModePlaying "01-training" `shouldBe` Just "overworld"

    it "unbound modes keep the current track (menu, dialogue, death)" $ do
      musicFor testDefs ModeMenu "01-training" `shouldBe` Nothing
      musicFor testDefs (ModeDialogue [("a", "b")]) "x" `shouldBe` Nothing
      musicFor testDefs (ModeDead 0.5) "x" `shouldBe` Nothing
