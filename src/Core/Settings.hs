-- | Game settings: a tiny key=value file at @config/settings.cfg@.
--   Parsing/formatting is pure (tested); only load/save touch IO.
module Core.Settings
  ( Settings(..)
  , FontChoice(..)
  , fontLabel
  , Language(..)
  , langCode
  , langLabel
  , defaultSettings
  , settingsRows
  , toggleSetting
  , parseSettings
  , formatSettings
  , loadSettings
  , saveSettings
  , settingsPath
  ) where

import Control.Exception (try, IOException)
import Data.Char (isSpace)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), takeDirectory)

-- | Which UI font the game renders text with. 'FontPixel' is the built-in
--   3x5 rect font (ASCII only, zero assets); 'FontChinese' is a bundled
--   Traditional-Chinese TrueType font rendered through SDL2_ttf.
data FontChoice = FontPixel | FontChinese
  deriving (Eq, Show, Enum, Bounded)

-- | The value shown on the settings row for each font.
fontLabel :: FontChoice -> String
fontLabel FontPixel   = "PIXEL (ASCII)"
fontLabel FontChinese = "NOTO 中文"

-- | The game's text language. Each constructor maps to a string table at
--   @assets/lang/CODE.lang@ (loaded at startup; switching takes effect on
--   the next launch because content definitions are compiled against the
--   table once). Adding a language = new constructor + new .lang file.
data Language = LangZhTW | LangEn
  deriving (Eq, Show, Enum, Bounded)

-- | The table file's base name.
langCode :: Language -> String
langCode LangZhTW = "zh-TW"
langCode LangEn   = "en"

-- | The value shown on the settings row.
langLabel :: Language -> String
langLabel LangZhTW = "正體中文"
langLabel LangEn   = "ENGLISH"

data Settings = Settings
  { setShowFps    :: !Bool
  , setFullscreen :: !Bool
  , setFont       :: !FontChoice
  , setLang       :: !Language
  } deriving (Eq, Show)

-- | The TTF font is the default: it is far more readable than the 3x5 pixel
--   font and covers Traditional Chinese, so content files can use Chinese
--   out of the box. 'FontPixel' stays as the retro ASCII-only option.
defaultSettings :: Settings
defaultSettings = Settings False False FontChinese LangZhTW

settingsPath :: FilePath
settingsPath = "config" </> "settings.cfg"

-- | The rows shown on the settings menu page, in cursor order: label and
--   displayed value. Extend this list (and 'toggleSetting') when adding a
--   setting.
settingsRows :: Settings -> [(String, String)]
settingsRows s =
  [ ("SHOW FPS",   onOff (setShowFps s))
  , ("FULLSCREEN", onOff (setFullscreen s))
  , ("FONT",       fontLabel (setFont s))
  , ("LANGUAGE",   langLabel (setLang s) <> "  (RESTART)")
  ]
  where onOff b = if b then "ON" else "OFF"

-- | Toggle/cycle the Nth settings row (same order as 'settingsRows').
toggleSetting :: Int -> Settings -> Settings
toggleSetting 0 s = s { setShowFps = not (setShowFps s) }
toggleSetting 1 s = s { setFullscreen = not (setFullscreen s) }
toggleSetting 2 s = s { setFont = cycleEnum (setFont s) }
toggleSetting 3 s = s { setLang = cycleEnum (setLang s) }
toggleSetting _ s = s

cycleEnum :: (Eq a, Enum a, Bounded a) => a -> a
cycleEnum x | x == maxBound = minBound
            | otherwise     = succ x

parseSettings :: String -> Settings
parseSettings raw = foldl apply defaultSettings (map entry (lines raw))
  where
    entry line = case break (== '=') line of
      (k, '=' : v) -> (trim k, trim v)
      _            -> ("", "")
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    apply s (k, v) = case k of
      "showFps"    -> s { setShowFps = v == "true" }
      "fullscreen" -> s { setFullscreen = v == "true" }
      "font"       -> case v of
        "pixel"   -> s { setFont = FontPixel }
        "chinese" -> s { setFont = FontChinese }
        _         -> s  -- unknown value: keep the default
      "lang"       -> case v of
        "zh-TW" -> s { setLang = LangZhTW }
        "en"    -> s { setLang = LangEn }
        _       -> s
      _            -> s

formatSettings :: Settings -> String
formatSettings s = unlines
  [ "showFps=" <> bool (setShowFps s)
  , "fullscreen=" <> bool (setFullscreen s)
  , "font=" <> fontKey (setFont s)
  , "lang=" <> langCode (setLang s)
  ]
  where
    bool b = if b then "true" else "false"
    fontKey FontPixel   = "pixel"
    fontKey FontChinese = "chinese"

-- | Load settings; a missing or unreadable file yields the defaults.
loadSettings :: IO Settings
loadSettings = do
  result <- try (readFile settingsPath) :: IO (Either IOException String)
  pure $ either (const defaultSettings) parseSettings result

saveSettings :: Settings -> IO ()
saveSettings s = do
  createDirectoryIfMissing True (takeDirectory settingsPath)
  writeFile settingsPath (formatSettings s)
