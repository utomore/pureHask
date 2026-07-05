module CodecSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (isInfixOf)
import Test.Hspec

import Core.Types
import Save.Codec
import Sim.Spawn (PlayerPersist(..))

sampleSave :: SaveGame
sampleSave = SaveGame
  { svVersion = currentSaveVersion
  , svLevel   = 1
  , svStats   = RunStats 3 5 754.2
  , svPlayer  = PlayerPersist
      { ppBackpack = M.fromList [(ItemId "gold-key", 1), (ItemId "potion-hp-s", 4)]
      , ppEquipped = M.fromList [(SlotWeapon, ItemId "sword-rusty")]
      , ppVitals   = Vitals 62 100 50 50 80 100
      , ppStats    = DerivedStats 5 1 1.15
      }
  , svQuests  = QuestLog
      (M.fromList [ (QuestId "find-key", QuestProgress QActive 1 0)
                  , (QuestId "done-one", QuestProgress QDone 2 3)
                  ])
      (S.fromList ["met-elder"])
  }

spec :: Spec
spec = do
  describe "encode/decode" $ do
    it "roundtrips a full save" $
      decodeSave (encodeSave sampleSave) `shouldBe` Right sampleSave

    it "roundtrips an empty-handed player" $ do
      let bare = sampleSave
            { svPlayer = PlayerPersist M.empty M.empty
                           (fullVitals 100 50 100) baseStats
            , svQuests = emptyQuestLog
            }
      decodeSave (encodeSave bare) `shouldBe` Right bare

    it "rejects a wrong version loudly" $ do
      let futuristic = encodeSave sampleSave { svVersion = 999 }
      decodeSave futuristic `shouldSatisfy` \r -> case r of
        Left err -> "version" `isInfixOf` err
        Right _  -> False

    it "rejects garbage" $
      decodeSave (BL8.pack "not json at all") `shouldSatisfy` \r -> case r of
        Left _  -> True
        Right _ -> False

    it "rejects a truncated document" $ do
      let cut = BL8.take 40 (encodeSave sampleSave)
      decodeSave cut `shouldSatisfy` \r -> case r of
        Left _  -> True
        Right _ -> False

  describe "slotSummary" $
    it "shows level and play time" $
      slotSummary sampleSave `shouldBe` "LV 2  12:34"

  describe "slotPath" $
    it "maps slot 0 to the autosave file" $ do
      slotPath 0 `shouldNotBe` slotPath 1
      slotPath 0 `shouldSatisfy` ("auto" `isInfixOf`)
