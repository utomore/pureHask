-- | S-expression syntax: the single external data language of this project.
--   Item definitions, quest scripts, NPC scripts and scene files all parse
--   through here (see docs/SYSTEMS_DESIGN.md §1).
--
--   Grammar (deliberately tiny):
--
--     * @(form …)@ — lists
--     * bare symbols: @item@, @gold-key@
--     * numbers: @42@, @-3@, @0.25@
--     * strings: @"hello"@ (supports @\"@ and @\\@ escapes)
--     * comments: @;@ to end of line
--
--   Pure, zero dependencies beyond @text@. Parse errors carry line numbers.
module Script.Sexp
  ( Sexp(..)
  , parseSexps
    -- * Accessors (schema helpers)
  , sexpSymbol
  , sexpString
  , sexpNum
  , sexpInt
  , headSymbol
  , formsNamed
  , fieldOf
  ) where

import Data.Char (isSpace)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | A parsed S-expression node.
data Sexp
  = SSym !Text     -- ^ bare symbol
  | SNum !Double   -- ^ numeric literal (ints and floats share this)
  | SStr !Text     -- ^ quoted string
  | SList ![Sexp]  -- ^ parenthesised form
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Tokenizer
--------------------------------------------------------------------------------

data Token = TLParen | TRParen | TSym !Text | TNum !Double | TStr !Text
  deriving (Eq, Show)

-- | Tokenize with line tracking for error messages.
tokenize :: Text -> Either String [(Int, Token)]
tokenize = go 1
  where
    go :: Int -> Text -> Either String [(Int, Token)]
    go line t = case T.uncons t of
      Nothing -> Right []
      Just (c, rest)
        | c == '\n' -> go (line + 1) rest
        | isSpace c -> go line rest
        | c == ';'  -> go line (T.dropWhile (/= '\n') rest)
        | c == '('  -> ((line, TLParen) :) <$> go line rest
        | c == ')'  -> ((line, TRParen) :) <$> go line rest
        | c == '"'  -> do
            (str, rest') <- takeString line rest
            ((line, TStr str) :) <$> go line rest'
        | otherwise ->
            let (word, rest') = T.span isWordChar t
            in if T.null word
                 then Left ("line " <> show line <> ": unexpected character " <> show c)
                 else do
                   tok <- classify line word
                   ((line, tok) :) <$> go line rest'

    isWordChar ch = not (isSpace ch) && ch `notElem` ("();\"" :: String)

    -- A word is a number only when it parses as one IN FULL; anything else
    -- (including digit-led names like "01-training") is a symbol.
    classify _line word =
      case reads (T.unpack word) :: [(Double, String)] of
        [(n, "")] -> Right (TNum n)
        _         -> Right (TSym word)

    takeString :: Int -> Text -> Either String (Text, Text)
    takeString line = loop []
      where
        loop acc t = case T.uncons t of
          Nothing -> Left ("line " <> show line <> ": unterminated string")
          Just ('"', rest) -> Right (T.pack (reverse acc), rest)
          Just ('\\', rest) -> case T.uncons rest of
            Just ('"', r)  -> loop ('"' : acc) r
            Just ('\\', r) -> loop ('\\' : acc) r
            Just ('n', r)  -> loop ('\n' : acc) r
            _ -> Left ("line " <> show line <> ": bad escape in string")
          Just (ch, rest) -> loop (ch : acc) rest

--------------------------------------------------------------------------------
-- Parser
--------------------------------------------------------------------------------

-- | Parse a whole file into its top-level forms.
parseSexps :: Text -> Either String [Sexp]
parseSexps input = do
  toks <- tokenize input
  (forms, leftover) <- parseMany toks
  case leftover of
    []            -> Right forms
    ((line, _):_) -> Left ("line " <> show line <> ": unexpected ')'")

parseMany :: [(Int, Token)] -> Either String ([Sexp], [(Int, Token)])
parseMany toks = case toks of
  []                  -> Right ([], [])
  ((_, TRParen) : _)  -> Right ([], toks)
  _ -> do
    (s, rest) <- parseOne toks
    (ss, rest') <- parseMany rest
    Right (s : ss, rest')

parseOne :: [(Int, Token)] -> Either String (Sexp, [(Int, Token)])
parseOne [] = Left "unexpected end of input"
parseOne ((line, tok) : rest) = case tok of
  TSym s  -> Right (SSym s, rest)
  TNum n  -> Right (SNum n, rest)
  TStr s  -> Right (SStr s, rest)
  TRParen -> Left ("line " <> show line <> ": unexpected ')'")
  TLParen -> do
    (items, rest') <- parseMany rest
    case rest' of
      ((_, TRParen) : rest'') -> Right (SList items, rest'')
      _ -> Left ("line " <> show line <> ": unclosed '('")

--------------------------------------------------------------------------------
-- Schema helpers
--------------------------------------------------------------------------------

sexpSymbol :: Sexp -> Maybe Text
sexpSymbol (SSym s) = Just s
sexpSymbol _        = Nothing

sexpString :: Sexp -> Maybe Text
sexpString (SStr s) = Just s
sexpString (SSym s) = Just s   -- allow bare words where prose is expected
sexpString _        = Nothing

sexpNum :: Sexp -> Maybe Double
sexpNum (SNum n) = Just n
sexpNum _        = Nothing

sexpInt :: Sexp -> Maybe Int
sexpInt (SNum n) | n == fromIntegral (round n :: Int) = Just (round n)
sexpInt _ = Nothing

-- | The leading symbol of a form: @headSymbol (item gold-key …) = Just "item"@.
headSymbol :: Sexp -> Maybe Text
headSymbol (SList (SSym s : _)) = Just s
headSymbol _                    = Nothing

-- | All sub-forms with the given head: @formsNamed "layer" scene@.
formsNamed :: Text -> [Sexp] -> [[Sexp]]
formsNamed name = mapMaybe match
  where
    match (SList (SSym s : rest)) | s == name = Just rest
    match _ = Nothing

-- | First field with the given head inside a form body:
--   @fieldOf "name" body = Just [SStr "GOLD KEY"]@.
fieldOf :: Text -> [Sexp] -> Maybe [Sexp]
fieldOf name body = case formsNamed name body of
  (x : _) -> Just x
  []      -> Nothing
