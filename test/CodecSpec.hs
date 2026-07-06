module CodecSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (isInfixOf)
import Test.Hspec

import Core.Types
import Save.Codec
import Sim.Spawn (PlayerPersist(..))

env :: SaveEnv
env = SaveEnv ["01-training", "02-ascent", "03-hollow"]

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

-- | A hand-written VERSION 1 save document (level as an index), as an old
--   install would have on disk.
v1Document :: BL8.ByteString
v1Document = BL8.pack $ concat
  [ "{\"version\":1,\"level\":1,"
  , "\"stats\":{\"deaths\":3,\"items\":5,\"time\":754.2},"
  , "\"player\":{\"backpack\":{\"gold-key\":1},\"equipped\":{},"
  , "\"vitals\":{\"hp\":62,\"maxHp\":100,\"mp\":50,\"maxMp\":50,"
  , "\"stamina\":80,\"maxStamina\":100},"
  , "\"derived\":{\"atk\":0,\"def\":0,\"speedMult\":1}},"
  , "\"quests\":{\"flags\":[],\"progress\":{}}}"
  ]

spec :: Spec
spec = do
  describe "encode/decode (v2)" $ do
    it "roundtrips a full save" $
      decodeSave env (encodeSave env sampleSave) `shouldBe` Right sampleSave

    it "roundtrips an empty-handed player" $ do
      let bare = sampleSave
            { svPlayer = PlayerPersist M.empty M.empty
                           (fullVitals 100 50 100) baseStats
            , svQuests = emptyQuestLog
            }
      decodeSave env (encodeSave env bare) `shouldBe` Right bare

    it "stores the level as a NAME so inserting levels cannot retarget saves" $ do
      let doc = BL8.unpack (encodeSave env sampleSave)
      doc `shouldSatisfy` ("02-ascent" `isInfixOf`)
      -- The same file decoded against a list with a NEW level inserted
      -- before it still finds the right level by name.
      let grown = SaveEnv ["01-training", "01b-secret", "02-ascent", "03-hollow"]
      svLevel <$> decodeSave grown (encodeSave env sampleSave) `shouldBe` Right 2

    it "rejects a save naming a level that no longer exists" $ do
      let shrunk = SaveEnv ["01-training"]
      decodeSave shrunk (encodeSave env sampleSave) `shouldSatisfy` \r -> case r of
        Left err -> "unknown level" `isInfixOf` err
        Right _  -> False

    it "rejects a FUTURE version loudly" $ do
      let futuristic = BL8.pack "{\"version\":999}"
      decodeSave env futuristic `shouldSatisfy` \r -> case r of
        Left err -> "newer" `isInfixOf` err
        Right _  -> False

    it "rejects garbage" $
      decodeSave env (BL8.pack "not json at all") `shouldSatisfy` \r -> case r of
        Left _  -> True
        Right _ -> False

    it "rejects a truncated document" $ do
      let cut = BL8.take 40 (encodeSave env sampleSave)
      decodeSave env cut `shouldSatisfy` \r -> case r of
        Left _  -> True
        Right _ -> False

  describe "migration v1 -> v2" $ do
    it "upgrades a v1 document (index becomes the level name)" $ do
      case decodeSave env v1Document of
        Right sv -> do
          svVersion sv `shouldBe` currentSaveVersion
          svLevel sv `shouldBe` 1
          svStats sv `shouldBe` RunStats 3 5 754.2
        Left err -> expectationFailure err

    it "a v1 index out of range is a loud error, not the wrong level" $ do
      let tiny = SaveEnv ["01-training"]
      decodeSave tiny v1Document `shouldSatisfy` \r -> case r of
        Left err -> "out of range" `isInfixOf` err
        Right _  -> False

  describe "sanitizeSave" $ do
    let knownQuests = [(QuestId "find-key", 2), (QuestId "done-one", 3)]
        knownItems = [ItemId "gold-key", ItemId "potion-hp-s", ItemId "sword-rusty"]

    it "a clean save passes through untouched" $
      sanitizeSave knownQuests knownItems sampleSave
        `shouldBe` (sampleSave, [])

    it "drops quests that no longer exist, with a warning" $ do
      let (sv, warns) = sanitizeSave [(QuestId "done-one", 3)] knownItems sampleSave
      M.member (QuestId "find-key") (qlQuests (svQuests sv)) `shouldBe` False
      warns `shouldSatisfy` any ("find-key" `isInfixOf`)

    it "clamps a stage index past the quest's stage count" $ do
      let shrunkQuests = [(QuestId "find-key", 1), (QuestId "done-one", 3)]
          (sv, warns) = sanitizeSave shrunkQuests knownItems sampleSave
      qpStage (qlQuests (svQuests sv) M.! QuestId "find-key") `shouldBe` 0
      warns `shouldSatisfy` any ("clamped" `isInfixOf`)

    it "drops unknown backpack items and unequips unknown gear" $ do
      let fewItems = [ItemId "gold-key"]
          (sv, warns) = sanitizeSave knownQuests fewItems sampleSave
      M.member (ItemId "potion-hp-s") (ppBackpack (svPlayer sv)) `shouldBe` False
      M.member SlotWeapon (ppEquipped (svPlayer sv)) `shouldBe` False
      length warns `shouldBe` 2

  describe "slotSummary" $
    it "shows level and play time" $
      slotSummary sampleSave `shouldBe` "LV 2  12:34"

  describe "slotPath" $
    it "maps slot 0 to the autosave file" $ do
      slotPath 0 `shouldNotBe` slotPath 1
      slotPath 0 `shouldSatisfy` ("auto" `isInfixOf`)