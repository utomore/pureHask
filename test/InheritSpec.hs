module InheritSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Linear (V2(..))
import Test.Hspec

import Core.Config (tileSize)
import Core.Types (EnemyId(..), NpcId(..))
import Enemy.Script
import Npc.Script
import Script.Expr (Cond(..))
import Script.Inherit
import Script.Sexp

parse :: String -> [[Sexp]]
parse src = case parseSexps (T.pack src) of
  Right forms -> formsNamed "enemy" forms
  Left err    -> error err

spec :: Spec
spec = do
  describe "resolveInherits" $ do
    it "a definition without (inherit …) passes through untouched" $ do
      let bodies = parse "(enemy a (name \"A\") (stats (hp 1)))"
      resolveInherits "enemy" ["stats"] [] [] bodies `shouldBe` Right bodies

    it "child fields override the base's wholesale" $ do
      let bodies = parse
            "(enemy a (color 1 2 3) (size 9 9))\
            \(enemy b (inherit a) (color 7 7 7))"
          Right [_, b] = resolveInherits "enemy" [] [] [] bodies
      b `shouldSatisfy` elem (SList [SSym "color", SNum 7, SNum 7, SNum 7])
      b `shouldSatisfy` elem (SList [SSym "size", SNum 9, SNum 9])
      b `shouldSatisfy` notElem (SList [SSym "color", SNum 1, SNum 2, SNum 3])

    it "merge fields merge per inner form (child wins per stat)" $ do
      let bodies = parse
            "(enemy a (stats (hp 30) (damage 10) (speed 60)))\
            \(enemy b (inherit a) (stats (hp 50)))"
          Right [_, b] = resolveInherits "enemy" ["stats"] [] [] bodies
          Just stats = lookup (Just "stats")
            [ (nameOf f, f) | f <- b ]
      stats `shouldBe` SList [ SSym "stats"
                             , SList [SSym "damage", SNum 10]
                             , SList [SSym "speed", SNum 60]
                             , SList [SSym "hp", SNum 50]
                             ]

    it "drop fields are never inherited" $ do
      let bodies = parse
            "(enemy a (spawn-at lvl 1 1) (color 1 1 1))\
            \(enemy b (inherit a) (spawn-at lvl 9 9))"
          Right [_, b] = resolveInherits "enemy" [] [] ["spawn-at"] bodies
      [ f | f <- b, nameOf f == Just "spawn-at" ]
        `shouldBe` [SList [SSym "spawn-at", SSym "lvl", SNum 9, SNum 9]]

    it "chains: a base that inherited is already expanded" $ do
      let bodies = parse
            "(enemy a (color 1 1 1) (size 2 2))\
            \(enemy b (inherit a) (color 5 5 5))\
            \(enemy c (inherit b))"
          Right [_, _, c] = resolveInherits "enemy" [] [] [] bodies
      c `shouldSatisfy` elem (SList [SSym "color", SNum 5, SNum 5, SNum 5])
      c `shouldSatisfy` elem (SList [SSym "size", SNum 2, SNum 2])

    it "an unknown or later base is a loud error" $ do
      let bodies = parse "(enemy b (inherit nope) (color 1 1 1))"
      resolveInherits "enemy" [] [] [] bodies `shouldSatisfy` \r -> case r of
        Left e  -> "nope" `T.isInfixOf` T.pack e
        Right _ -> False

    it "multiple (inherit …) forms are rejected" $ do
      let bodies = parse
            "(enemy a (color 1 1 1))\
            \(enemy b (inherit a) (inherit a))"
      resolveInherits "enemy" [] [] [] bodies `shouldSatisfy` \r -> case r of
        Left e  -> "multiple" `T.isInfixOf` T.pack e
        Right _ -> False

    it "rule fields concatenate child-first; the base's default survives" $ do
      let bodies = parse
            "(enemy a (dialogue (rule (flag x) (say a \"P\")) (default (say a \"D\"))))\
            \(enemy b (inherit a) (dialogue (rule (flag y) (say b \"C\"))))"
          Right [_, b] = resolveInherits "enemy" [] ["dialogue"] [] bodies
          Just dlg = lookup (Just "dialogue") [ (nameOf f, f) | f <- b ]
          SList (_ : rules) = dlg
      map nameOf rules `shouldBe` [Just "rule", Just "rule", Just "default"]
      -- the child's rule comes FIRST (highest priority)
      head rules `shouldSatisfy` \r -> case r of
        SList (_ : SList [SSym "flag", SSym "y"] : _) -> True
        _ -> False

    it "the child's (default …) replaces the base's" $ do
      let bodies = parse
            "(enemy a (dialogue (rule (flag x) (say a \"P\")) (default (say a \"OLD\"))))\
            \(enemy b (inherit a) (dialogue (default (say b \"NEW\"))))"
          Right [_, b] = resolveInherits "enemy" [] ["dialogue"] [] bodies
          Just dlg = lookup (Just "dialogue") [ (nameOf f, f) | f <- b ]
          SList (_ : rules) = dlg
      -- child default first, base rule kept, base default gone
      map nameOf rules `shouldBe` [Just "default", Just "rule"]

  describe "end to end through the enemy compiler" $
    it "slime-red keeps the base speed/behavior, overrides hp/damage/color" $ do
      let src = unlines
            [ "(enemy slime (name \"S\") (color 90 200 90) (size 24 18)"
            , "  (stats (hp 30) (damage 10) (speed 60))"
            , "  (behavior (patrol 3) (aggro 150) (attack contact))"
            , "  (spawn-at 03-hollow 20 13))"
            , "(enemy slime-red (inherit slime) (name \"R\")"
            , "  (color 220 80 80) (stats (hp 50) (damage 14))"
            , "  (spawn-at 05-abyssgate 30 13))"
            ]
          Right forms = parseSexps (T.pack src)
      case compileEnemies M.empty forms of
        Right [base, red] -> do
          edId red `shouldBe` EnemyId "slime-red"
          edHp red `shouldBe` 50.0
          edDamage red `shouldBe` 14.0
          edSpeed red `shouldBe` 60.0                -- inherited
          edColor red `shouldBe` (220, 80, 80)
          edSize red `shouldBe` edSize base          -- inherited
          edTree red `shouldBe` edTree base          -- inherited behavior
          edSpawns red `shouldBe`
            [("05-abyssgate", V2 (30.0 * tileSize) (13.0 * tileSize))]
        other -> expectationFailure (show (fmap (map edId) other))

  npcSpec

npcSpec :: Spec
npcSpec =
  describe "end to end through the npc compiler" $
    it "echo inherits the base's rules below its own; movement/spawn are its own" $ do
      let src = unlines
            [ "(npc elder (name \"E\") (color 200 180 120)"
            , "  (spawn-at 01-a 10 13)"
            , "  (movement (patrol 8 14) (speed 35) (pause 1.5))"
            , "  (dialogue"
            , "    (rule (flag met) (say elder \"AGAIN\"))"
            , "    (default (say elder \"HELLO\") (set-flag met))))"
            , "(npc echo (inherit elder) (name \"E2\")"
            , "  (spawn-at 05-b 44 13)"
            , "  (movement (idle))"
            , "  (dialogue (rule (flag done) (say echo \"PRAISE\"))))"
            ]
          Right forms = parseSexps (T.pack src)
      case compileNpcs M.empty forms of
        Right [_, echo] -> do
          ndId echo `shouldBe` NpcId "echo"
          ndSpawn echo `shouldBe` V2 (44 * tileSize) (13 * tileSize)
          ndMovement echo `shouldBe` MoveIdle
          -- child's rule first, then the base's rule and default
          map drCond (ndDialogue echo) `shouldBe`
            [Just (CFlag "done"), Just (CFlag "met"), Nothing]
        other -> expectationFailure (show (fmap (map ndId) other))

nameOf :: Sexp -> Maybe T.Text
nameOf (SList (SSym n : _)) = Just n
nameOf _                    = Nothing