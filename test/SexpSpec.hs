module SexpSpec (spec) where

import Data.List (isInfixOf)
import Test.Hspec

import Script.Sexp

spec :: Spec
spec = do
  describe "parseSexps" $ do
    it "parses symbols, numbers and strings" $
      parseSexps "(item gold-key 42 -3 0.25 \"HELLO\")"
        `shouldBe` Right
          [SList [SSym "item", SSym "gold-key", SNum 42, SNum (-3), SNum 0.25, SStr "HELLO"]]

    it "parses nested forms" $
      parseSexps "(a (b (c 1)) d)"
        `shouldBe` Right [SList [SSym "a", SList [SSym "b", SList [SSym "c", SNum 1]], SSym "d"]]

    it "parses multiple top-level forms" $
      parseSexps "(a) (b)" `shouldBe` Right [SList [SSym "a"], SList [SSym "b"]]

    it "skips comments" $
      parseSexps "; header\n(a 1) ; trailing\n(b)"
        `shouldBe` Right [SList [SSym "a", SNum 1], SList [SSym "b"]]

    it "handles string escapes" $
      parseSexps "(\"a\\\"b\\\\c\")" `shouldBe` Right [SList [SStr "a\"b\\c"]]

    it "treats a dash-word as a symbol, not a number" $
      parseSexps "(- -x)" `shouldBe` Right [SList [SSym "-", SSym "-x"]]

    it "reports unclosed parens with a line number" $
      parseSexps "\n\n(a (b)" `shouldSatisfy` \r -> case r of
        Left err -> "line 3" `isInfixOf` err
        Right _  -> False

    it "reports stray closing paren" $
      parseSexps "(a))" `shouldSatisfy` isLeft

    it "reports unterminated string" $
      parseSexps "(\"abc" `shouldSatisfy` isLeft

  describe "accessors" $ do
    it "formsNamed finds all matching sub-forms" $ do
      let Right forms = parseSexps "(item a) (npc x) (item b)"
      length (formsNamed "item" forms) `shouldBe` 2

    it "fieldOf finds the first field body" $ do
      let Right [SList (_ : body)] = parseSexps "(item gold (name \"KEY\") (name \"DUP\"))"
      (fieldOf "name" body >>= safeHead >>= sexpString) `shouldBe` Just "KEY"

    it "sexpInt rejects fractional numbers" $ do
      sexpInt (SNum 2.5) `shouldBe` Nothing
      sexpInt (SNum 3.0) `shouldBe` Just 3
  where
    isLeft :: Either a b -> Bool
    isLeft (Left _) = True
    isLeft _        = False
    safeHead (x : _) = Just x
    safeHead []      = Nothing
