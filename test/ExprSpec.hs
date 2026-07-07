module ExprSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import Linear (V2(..))
import Test.Hspec

import Core.Config (tileSize)
import Core.Types
import Script.Expr
import Script.Sexp (parseSexps)

-- | Compile a single condition from source.
cond :: String -> Cond
cond src = case parseSexps (T.pack src) of
  Right [form] -> either error id (compileCond form)
  other        -> error ("bad test source " <> show other)

action :: String -> Either String Action
action src = case parseSexps (T.pack src) of
  Right [form] -> compileAction M.empty form
  other        -> Left ("bad test source " <> show other)

env :: ScriptEnv
env = emptyScriptEnv
  { envFlags = S.fromList ["met-elder"]
  , envBackpack = M.fromList [(ItemId "gold-key", 1), (ItemId "potion-hp-s", 3)]
  , envQuestPhase = M.fromList [(QuestId "find-key", QActive)]
  , envPlayerPos = V2 (10.0 * tileSize) (5.0 * tileSize)
  , envLevelName = "01-training"
  }

spec :: Spec
spec = do
  describe "compileCond + evalCond" $ do
    it "flag conditions" $ do
      evalCond env (cond "(flag met-elder)") `shouldBe` True
      evalCond env (cond "(flag unknown)") `shouldBe` False
      evalCond env (cond "(not (flag unknown))") `shouldBe` True

    it "boolean combinators" $ do
      evalCond env (cond "(and (flag met-elder) (has-item gold-key))") `shouldBe` True
      evalCond env (cond "(and (flag met-elder) (flag nope))") `shouldBe` False
      evalCond env (cond "(or (flag nope) (flag met-elder))") `shouldBe` True

    it "item count conditions" $ do
      evalCond env (cond "(has-item potion-hp-s 3)") `shouldBe` True
      evalCond env (cond "(has-item potion-hp-s 4)") `shouldBe` False
      evalCond env (cond "(has-item missing)") `shouldBe` False

    it "quest state conditions default to available" $ do
      evalCond env (cond "(quest-state find-key active)") `shouldBe` True
      evalCond env (cond "(quest-state find-key done)") `shouldBe` False
      evalCond env (cond "(quest-state other available)") `shouldBe` True

    it "player-near uses tile coordinates" $ do
      evalCond env (cond "(player-near 10 5 1)") `shouldBe` True
      evalCond env (cond "(player-near 20 5 2)") `shouldBe` False

    it "level-is matches the current level" $ do
      evalCond env (cond "(level-is 01-training)") `shouldBe` True
      evalCond env (cond "(level-is 02-ascent)") `shouldBe` False

    it "rejects unknown condition forms" $ do
      case parseSexps (T.pack "(frobnicate 3)") of
        Right [form] -> compileCond form `shouldSatisfy` isLeft
        _            -> expectationFailure "parse failed"

  describe "compileAction" $ do
    it "compiles item actions with a default count of 1" $ do
      action "(give-item gold-key)" `shouldBe` Right (AGiveItem (ItemId "gold-key") 1)
      action "(take-item gold-key 2)" `shouldBe` Right (ATakeItem (ItemId "gold-key") 2)

    it "compiles dialogue with multiple lines" $
      action "(say elder \"HELLO\" \"TRAVELLER\")"
        `shouldBe` Right (ASay "elder" ["HELLO", "TRAVELLER"])

    it "compiles quest actions" $
      action "(offer-quest find-key)" `shouldBe` Right (AOfferQuest (QuestId "find-key"))

    it "compiles npc actions with tile coordinates" $
      action "(spawn-npc elder 12 8)" `shouldBe` Right (ASpawnNpc (NpcId "elder") 12 8)

    it "rejects unknown actions" $
      action "(explode-world)" `shouldSatisfy` isLeft

  describe "reference collection" $ do
    it "collects item ids from nested conditions" $
      condItemRefs (cond "(and (has-item a) (or (has-item b 2) (flag x)))")
        `shouldBe` [ItemId "a", ItemId "b"]

    it "collects quest ids from actions" $ do
      let Right a = action "(complete-quest find-key)"
      actionQuestRefs a `shouldBe` [QuestId "find-key"]
  where
    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft _        = False
