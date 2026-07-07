module QuestSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Test.Hspec

import Core.Types
import Quest.Runtime
import Quest.Script
import Script.Sexp (parseSexps, formsNamed)

-- | A two-stage quest: collect two keys, then reach the goal.
testDefs :: [QuestDef]
testDefs =
  let src = "(quest two-keys\
            \  (name \"TWO KEYS\")\
            \  (auto-start)\
            \  (stage collect\
            \    (goal \"COLLECT 2 KEYS\")\
            \    (objective (collect gold-key 2))\
            \    (on-complete (toast \"GOT THEM\") (advance-quest two-keys)))\
            \  (stage escape\
            \    (goal \"REACH THE EXIT\")\
            \    (objective (reach-goal))\
            \    (on-complete (give-item potion-hp-s 1) (set-flag done-flag)\
            \                 (complete-quest two-keys))))\
            \(quest side-talk\
            \  (name \"SMALL TALK\")\
            \  (stage talk\
            \    (goal \"TALK TO THE ELDER\")\
            \    (objective (talk-to elder))\
            \    (on-complete (complete-quest side-talk))))"
      Right forms = parseSexps (T.pack src)
      Right defs = mapM (compileQuest M.empty) (formsNamed "quest" forms)
  in defs

qid :: QuestId
qid = QuestId "two-keys"

phaseOf :: QuestLog -> QuestId -> QuestPhase
phaseOf qlog q = qpPhase (M.findWithDefault (QuestProgress QAvailable 0 0) q (qlQuests qlog))

spec :: Spec
spec = do
  describe "compileQuest" $ do
    it "compiles stages in order" $ do
      let qd = head testDefs
      qdAuto qd `shouldBe` True
      map qsName (qdStages qd) `shouldBe` ["collect", "escape"]

    it "collects item references for validation" $
      questItemRefs (head testDefs)
        `shouldMatchList` [ItemId "gold-key", ItemId "potion-hp-s"]

    it "rejects a quest without stages" $ do
      let Right [form] = parseSexps "(quest empty (name \"X\"))"
          [body] = formsNamed "quest" [form]
      compileQuest M.empty body `shouldSatisfy` \r -> case r of
        Left _  -> True
        Right _ -> False

  describe "stepQuests" $ do
    let log0 = initialQuestLog testDefs

    it "auto-start quests begin active; others stay available" $ do
      phaseOf log0 qid `shouldBe` QActive
      phaseOf log0 (QuestId "side-talk") `shouldBe` QAvailable

    it "counting objectives track partial progress" $ do
      let out = stepQuests defaultQuestText testDefs [EvItemPicked (ItemId "gold-key")] log0
      qpCount (qlQuests (qoLog out) M.! qid) `shouldBe` 1
      phaseOf (qoLog out) qid `shouldBe` QActive

    it "unrelated pickups do not progress the objective" $ do
      let out = stepQuests defaultQuestText testDefs [EvItemPicked (ItemId "potion-hp-s")] log0
      qpCount (qlQuests (qoLog out) M.! qid) `shouldBe` 0

    it "fulfilling a stage advances to the next and toasts" $ do
      let out = stepQuests defaultQuestText testDefs
                  [EvItemPicked (ItemId "gold-key"), EvItemPicked (ItemId "gold-key")]
                  log0
      qpStage (qlQuests (qoLog out) M.! qid) `shouldBe` 1
      qoToasts out `shouldSatisfy` elem "GOT THEM"

    it "completing the final stage rewards, flags and finishes" $ do
      let mid = qoLog (stepQuests defaultQuestText testDefs
                  [EvItemPicked (ItemId "gold-key"), EvItemPicked (ItemId "gold-key")]
                  log0)
          out = stepQuests defaultQuestText testDefs [EvGoalReached] mid
      phaseOf (qoLog out) qid `shouldBe` QDone
      qoCommands out `shouldBe` [WcGiveItem (ItemId "potion-hp-s") 1]
      S.member "done-flag" (qlFlags (qoLog out)) `shouldBe` True
      qoToasts out `shouldSatisfy` elem "任務完成:TWO KEYS"

    it "talk-to objectives complete on the matching npc" $ do
      let activated = qoLog (stepQuests defaultQuestText testDefs [] log0)
          started = stepQuests defaultQuestText testDefs [EvTalkedTo (NpcId "stranger")]
                      activated { qlQuests = M.insert (QuestId "side-talk")
                                    (QuestProgress QActive 0 0)
                                    (qlQuests activated) }
      phaseOf (qoLog started) (QuestId "side-talk") `shouldBe` QActive
      let done = stepQuests defaultQuestText testDefs [EvTalkedTo (NpcId "elder")] (qoLog started)
      phaseOf (qoLog done) (QuestId "side-talk") `shouldBe` QDone

    it "a restored run replaces the whole log" $ do
      let saved = QuestLog (M.fromList [(qid, QuestProgress QDone 1 0)])
                           (S.fromList ["loaded"])
          out = stepQuests defaultQuestText testDefs [EvRunRestored 1 emptyRunStats saved] log0
      qoLog out `shouldBe` saved

  describe "activeGoal" $ do
    it "reports the current stage goal of the first active quest" $
      activeGoal testDefs (initialQuestLog testDefs)
        `shouldBe` Just ("TWO KEYS", "COLLECT 2 KEYS")

    it "reports nothing when no quest is active" $
      activeGoal testDefs emptyQuestLog `shouldBe` Nothing
