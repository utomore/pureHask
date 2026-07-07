module LangSpec (spec) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

import Core.Lang
import Script.Sexp (Sexp(..))

table :: LangTable
table = M.fromList [("item.key.name", "金鑰匙"), ("ui.hello", "你好")]

spec :: Spec
spec = do
  describe "parseLang" $ do
    it "parses key = value lines, trimming whitespace" $
      parseLang "  a.b = 哈囉 \nc.d=X"
        `shouldBe` Right (M.fromList [("a.b", "哈囉"), ("c.d", "X")])

    it "ignores comments and blank lines" $
      parseLang ";; c\n# c\n\n a = 1"
        `shouldBe` Right (M.fromList [("a", "1")])

    it "values may contain '='" $
      parseLang "eq = a = b"
        `shouldBe` Right (M.fromList [("eq", "a = b")])

    it "rejects a line without '='" $
      parseLang "not a pair" `shouldSatisfy` \r -> case r of
        Left e  -> "line 1" `elem` words e || not (null e)
        Right _ -> False

    it "later duplicates win" $
      parseLang "k = old\nk = new"
        `shouldBe` Right (M.fromList [("k", "new")])

  describe "langText" $ do
    it "string literals pass through untouched" $
      langText table (SStr "字面值") `shouldBe` Right "字面值"

    it "symbols resolve as keys" $
      langText table (SSym "item.key.name") `shouldBe` Right "金鑰匙"

    it "unknown keys are an error naming the key" $
      langText table (SSym "item.bogus") `shouldSatisfy` \r -> case r of
        Left e  -> "item.bogus" `T.isInfixOf` T.pack e
        Right _ -> False

    it "non-text forms are an error" $
      langText table (SNum 3) `shouldSatisfy` \r -> case r of
        Left _  -> True
        Right _ -> False

  describe "langLookup" $ do
    it "finds engine ui keys" $
      langLookup table "ui.hello" `shouldBe` Right "你好"

    it "reports missing ui keys" $
      langLookup table "ui.missing" `shouldSatisfy` \r -> case r of
        Left e  -> "ui.missing" `T.isInfixOf` T.pack e
        Right _ -> False