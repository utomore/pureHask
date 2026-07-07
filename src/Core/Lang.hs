-- | The string table: display text lives in @assets/lang/LANG.lang@ files,
--   content files reference it by key. This is the i18n seam — switching
--   language swaps one table; content and logic never change.
--
--   File format (UTF-8, one entry per line):
--
--   > ;; comment
--   > item.gold-key.name = 金鑰匙
--   > npc.elder.hello.1  = 歡迎,旅行者。
--
--   In S-expression content files, every display-text position accepts
--   either a literal string @"文字"@ (fine for prototyping) or a bare
--   symbol @item.gold-key.name@ which is resolved against the table at
--   startup — an unknown key aborts the launch (fail loudly, like every
--   other dangling reference).
module Core.Lang
  ( LangTable
  , emptyLang
  , parseLang
  , loadLang
  , langText
  , langLookup
  ) where

import qualified Data.ByteString as BS
import Data.Char (isSpace)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')

import Script.Sexp

-- | key -> translated text.
type LangTable = M.Map Text Text

emptyLang :: LangTable
emptyLang = M.empty

-- | Parse a @.lang@ file: @key = value@ lines; @;;@/@#@ comments and blank
--   lines are ignored. Later duplicates win (documented; useful for patch
--   files if that is ever needed).
parseLang :: Text -> Either String LangTable
parseLang raw = M.fromList . concat <$> mapM entry (zip [1 :: Int ..] (T.lines raw))
  where
    entry (n, line)
      | T.null trimmed = Right []
      | ";;" `T.isPrefixOf` trimmed = Right []
      | "#"  `T.isPrefixOf` trimmed = Right []
      | otherwise = case T.breakOn "=" trimmed of
          (k, rest)
            | Just v <- T.stripPrefix "=" rest
            , not (T.null (T.strip k)) ->
                Right [(T.strip k, T.strip v)]
          _ -> Left ("lang line " <> show n <> ": expected 'key = value', got "
                     <> show (T.unpack trimmed))
      where trimmed = T.dropWhile isSpace (T.dropWhileEnd isSpace line)

-- | Load a language file; missing file or bad UTF-8 is a startup failure.
loadLang :: FilePath -> IO (Either String LangTable)
loadLang path = do
  bytes <- BS.readFile path
  pure $ case decodeUtf8' bytes of
    Left err  -> Left (path <> ": not valid UTF-8 (" <> show err <> ")")
    Right txt -> either (Left . ((path <> ": ") <>)) Right (parseLang txt)

-- | Resolve one display-text position in a content file: a string literal
--   passes through, a symbol is looked up as a key.
langText :: LangTable -> Sexp -> Either String Text
langText table form = case form of
  SStr s -> Right s
  SSym k -> case M.lookup k table of
    Just v  -> Right v
    Nothing -> Left ("unknown text key '" <> T.unpack k
                     <> "' (add it to the assets/lang/*.lang files)")
  other  -> Left ("expected a string literal or a text key, got " <> show other)

-- | Look up an engine-side UI key (quest toasts and the like). These keys
--   are required — the caller decides whether missing is fatal.
langLookup :: LangTable -> Text -> Either String Text
langLookup table k = case M.lookup k table of
  Just v  -> Right v
  Nothing -> Left ("unknown ui text key '" <> T.unpack k <> "'")
