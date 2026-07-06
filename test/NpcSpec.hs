module NpcSpec (spec) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Linear (V2(..))
import Test.Hspec

import Core.Config (tileSize)
import Core.Types
import Npc.Core
import Npc.Script
import Script.Expr
import Script.Sexp (parseSexps, formsNamed)

elderDef :: NpcDef
elderDef =
  let src = "(npc elder\
            \  (name \"ELDER\")\
            \  (spawn-at 01-training 10 13)\
            \  (movement (patrol 8 14) (speed 35) (pause 1.5))\
            \  (chatter 9 \"LINE A\" \"LINE B\")\
            \  (dialogue\
            \    (rule (flag met-elder) (say elder \"AGAIN\"))\
            \    (default (say elder \"HELLO\" \"FIND THE KEY\") (set-flag met-elder))))"
      Right forms = parseSexps (T.pack src)
      Right [def] = mapM (compileNpc M.empty) (formsNamed "npc" forms)
  in def

spec :: Spec
spec = do
  describe "compileNpc" $ do
    it "converts spawn and patrol coordinates from tiles to pixels" $ do
      ndSpawn elderDef `shouldBe` V2 (10 * tileSize) (13 * tileSize)
      ndMovement elderDef
        `shouldBe` MovePatrol (8 * tileSize) (14 * tileSize) 35 1.5

    it "keeps dialogue rules in file order, default last" $ do
      length (ndDialogue elderDef) `shouldBe` 2
      drCond (last (ndDialogue elderDef)) `shouldBe` Nothing

    it "rejects an npc without spawn-at" $ do
      let Right forms = parseSexps "(npc x (name \"X\"))"
          [body] = formsNamed "npc" forms
      compileNpc M.empty body `shouldSatisfy` \r -> case r of
        Left err -> "spawn-at" `elem` words err || not (null err)
        Right _  -> False

  describe "stepNpcAi (patrol)" $ do
    let ai0 = initialAi elderDef
        atX x = V2 (x * tileSize) (13 * tileSize)

    it "walks right until the patrol end" $ do
      let (_, vx) = stepNpcAi elderDef 0.01 (atX 10) ai0
      vx `shouldBe` 35

    it "turns around and pauses at the right end" $ do
      let (ai1, vx) = stepNpcAi elderDef 0.01 (atX 14) ai0
      vx `shouldBe` 0
      naDir ai1 `shouldBe` DirLeft
      naPauseLeft ai1 `shouldSatisfy` (> 0)

    it "resumes walking left after the pause" $ do
      let (ai1, _) = stepNpcAi elderDef 0.01 (atX 14) ai0
          (_, vx) = stepNpcAi elderDef 2.0 (atX 14) ai1  -- pause expires
          (_, vx') = stepNpcAi elderDef 0.01 (atX 13)
                       ai1 { naPauseLeft = 0 }
      -- Either the frame after expiry or a fresh unpaused state moves left.
      (vx <= 0 && vx' == -35) `shouldBe` True

    it "chatter bubbles appear when the timer fires and expire later" $ do
      let (ai1, _) = stepNpcAi elderDef 9.5 (atX 10) ai0  -- interval is 9
      fmap fst (naBubble ai1) `shouldBe` Just "LINE A"
      let (ai2, _) = stepNpcAi elderDef 3.5 (atX 10) ai1  -- bubble life is 3
      naBubble ai2 `shouldBe` Nothing

  describe "evalDialogue" $ do
    it "the default rule fires for a stranger" $ do
      let acts = evalDialogue emptyScriptEnv elderDef
      acts `shouldSatisfy` any (\a -> case a of ASay _ _ -> True; _ -> False)
      acts `shouldSatisfy` elem (ASetFlag "met-elder")

    it "a matching condition takes priority over the default" $ do
      let env = emptyScriptEnv { envFlags = S.fromList ["met-elder"] }
      evalDialogue env elderDef `shouldBe` [ASay "elder" ["AGAIN"]]
