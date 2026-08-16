module TalentSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

import Core.Config (talentKillsPerPoint)
import Core.Types
import Script.Sexp (parseSexps)
import Talent.Core
import Talent.Script

-- | A small tree: two blade nodes (the second gated on the first) and one
--   independent defence node.
testDefs :: [TalentDef]
testDefs =
  let Right defs = parseSexps (T.pack src) >>= compileTalents M.empty
  in defs
  where
    src = "(talent blade-touch (name \"KEEN TOUCH\") (max-rank 3) (cost 1)\
          \  (effect (atk 2)))\
          \(talent blade-edge (name \"RENDING EDGE\") (max-rank 2) (cost 2)\
          \  (requires blade-touch 2) (effect (atk 4) (spd 5)))\
          \(talent bulwark (name \"BULWARK\") (max-rank 2) (cost 1)\
          \  (effect (def 1) (max-hp 25) (max-stamina 10)))"

withPoints :: Int -> TalentState
withPoints n = TalentState n M.empty

spec :: Spec
spec = do
  describe "Talent.Script.compileTalents" $ do
    it "compiles the tree in file order" $
      map tdId testDefs
        `shouldBe` map TalentId ["blade-touch", "blade-edge", "bulwark"]

    it "rejects a duplicate id" $ do
      let src = "(talent a (name \"A\") (effect (atk 1)))\
                \(talent a (name \"A2\") (effect (atk 1)))"
          r = parseSexps (T.pack src) >>= compileTalents M.empty
      r `shouldSatisfy` either (const True) (const False)

    it "rejects a forward (requires …) reference" $ do
      let src = "(talent a (name \"A\") (requires b) (effect (atk 1)))\
                \(talent b (name \"B\") (effect (atk 1)))"
          r = parseSexps (T.pack src) >>= compileTalents M.empty
      r `shouldSatisfy` either (const True) (const False)

    it "rejects an unknown effect" $ do
      let src = "(talent a (name \"A\") (effect (crit 5)))"
          r = parseSexps (T.pack src) >>= compileTalents M.empty
      r `shouldSatisfy` either (const True) (const False)

    it "rejects a node without effects" $ do
      let src = "(talent a (name \"A\"))"
          r = parseSexps (T.pack src) >>= compileTalents M.empty
      r `shouldSatisfy` either (const True) (const False)

  describe "canLearn / learnTalent" $ do
    it "buys a rank and deducts the cost" $ do
      let Just ts = learnTalent testDefs (TalentId "blade-touch") (withPoints 2)
      tsPoints ts `shouldBe` 1
      rankOf ts (TalentId "blade-touch") `shouldBe` 1

    it "refuses without enough points" $
      learnTalent testDefs (TalentId "blade-edge") (withPoints 1)
        `shouldBe` Nothing

    it "refuses past max rank" $ do
      let ts = TalentState 5 (M.fromList [(TalentId "blade-touch", 3)])
      learnTalent testDefs (TalentId "blade-touch") ts `shouldBe` Nothing

    it "gates on the prerequisite rank" $ do
      let low  = TalentState 5 (M.fromList [(TalentId "blade-touch", 1)])
          met  = TalentState 5 (M.fromList [(TalentId "blade-touch", 2)])
      canLearn testDefs low (TalentId "blade-edge") `shouldBe` False
      canLearn testDefs met (TalentId "blade-edge") `shouldBe` True

    it "refuses an unknown talent" $
      learnTalent testDefs (TalentId "nope") (withPoints 9) `shouldBe` Nothing

  describe "respecTalents" $ do
    it "refunds every spent point" $ do
      let ts = TalentState 1 (M.fromList [ (TalentId "blade-touch", 3)
                                         , (TalentId "blade-edge", 2) ])
      -- 3 ranks at cost 1 + 2 ranks at cost 2 = 7 refunded, +1 unspent
      respecTalents testDefs ts `shouldBe` TalentState 8 M.empty

    it "is a no-op on an empty tree" $
      respecTalents testDefs (withPoints 4) `shouldBe` withPoints 4

  describe "talentBonus" $ do
    it "is the baseline with nothing learned" $
      talentBonus testDefs emptyTalentState `shouldBe` baseStats

    it "multiplies effects by the learned rank and sums nodes" $ do
      let ts = TalentState 0 (M.fromList [ (TalentId "blade-touch", 2)
                                         , (TalentId "blade-edge", 1)
                                         , (TalentId "bulwark", 2) ])
          ds = talentBonus testDefs ts
      dsAtk ds `shouldBe` 8          -- 2*2 + 4
      dsDef ds `shouldBe` 2
      dsSpeedMult ds `shouldBe` 1.05
      dsMaxHp ds `shouldBe` 50
      dsMaxStamina ds `shouldBe` 20

  describe "killPointsBetween" $ do
    it "grants one point when the milestone is crossed" $
      killPointsBetween (talentKillsPerPoint - 1) talentKillsPerPoint
        `shouldBe` 1

    it "grants nothing inside a milestone window" $
      killPointsBetween 1 (talentKillsPerPoint - 1) `shouldBe` 0

    it "grants multiple points when several milestones pass at once" $
      killPointsBetween 0 (3 * talentKillsPerPoint) `shouldBe` 3

  describe "talentRows" $ do
    it "reports rank and buyability per row in definition order" $ do
      let ts = TalentState 1 (M.fromList [(TalentId "blade-touch", 2)])
      talentRows testDefs ts `shouldBe`
        [ (TalentId "blade-touch", 2, True)   -- affordable, below max
        , (TalentId "blade-edge", 0, False)   -- prereq met but cost 2 > 1 point
        , (TalentId "bulwark", 0, True)
        ]
