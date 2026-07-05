-- | Game settings: a tiny key=value file at @config/settings.cfg@.
--   Parsing/formatting is pure (tested); only load/save touch IO.
module Core.Settings
  ( Settings(..)
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

data Settings = Settings
  { setShowFps    :: !Bool
  , setFullscreen :: !Bool
  } deriving (Eq, Show)

defaultSettings :: Settings
defaultSettings = Settings False False

settingsPath :: FilePath
settingsPath = "config" </> "settings.cfg"

-- | The rows shown on the settings menu page, in cursor order. Extend this
--   list (and 'toggleSetting') when adding a setting.
settingsRows :: Settings -> [(String, Bool)]
settingsRows s =
  [ ("SHOW FPS",   setShowFps s)
  , ("FULLSCREEN", setFullscreen s)
  ]

-- | Toggle the Nth settings row (same order as 'settingsRows').
toggleSetting :: Int -> Settings -> Settings
toggleSetting 0 s = s { setShowFps = not (setShowFps s) }
toggleSetting 1 s = s { setFullscreen = not (setFullscreen s) }
toggleSetting _ s = s

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
      _            -> s

formatSettings :: Settings -> String
formatSettings s = unlines
  [ "showFps=" <> bool (setShowFps s)
  , "fullscreen=" <> bool (setFullscreen s)
  ]
  where bool b = if b then "true" else "false"

-- | Load settings; a missing or unreadable file yields the defaults.
loadSettings :: IO Settings
loadSettings = do
  result <- try (readFile settingsPath) :: IO (Either IOException String)
  pure $ either (const defaultSettings) parseSettings result

saveSettings :: Settings -> IO ()
saveSettings s = do
  createDirectoryIfMissing True (takeDirectory settingsPath)
  writeFile settingsPath (formatSettings s)
