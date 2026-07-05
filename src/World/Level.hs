-- | Level definitions and their parsing.
--
--   Levels are plain-text files under @assets/levels/@, loaded at startup and
--   sorted by file name — adding a level to the game is: drop a new @NN-name.txt@
--   file in that directory. No recompilation, no code change.
--
--   Marker legend (one character per tile):
--
--     * @#@ — solid block
--     * @P@ — player spawn (required, exactly the first one is used)
--     * @G@ — goal (required)
--     * item markers — see 'itemMarkers' (K, H, S, B, …)
--     * space — empty
--
--   To add a new item marker: define the item in @assets/items/items.def@
--   and map a character in 'itemMarkers'. For non-item entities, extend
--   'parseLevel' and teach "Sim.Spawn" and the renderer about them.
module World.Level
  ( LevelData(..)
  , parseLevel
  , loadLevelFile
  , discoverLevels
  , levelDirectory
  ) where

import qualified Data.Vector as V
import Data.Char (isSpace)
import Data.List (sort, isSuffixOf)
import Linear (V2(..))
import System.Directory (listDirectory)
import System.FilePath ((</>), takeBaseName)

import Core.Config (tileSize)
import Core.Types (ItemId(..))
import World.Tilemap

-- | Everything the game needs to instantiate a level.
data LevelData = LevelData
  { ldName        :: !String
  , ldTilemap     :: !Tilemap
  , ldPlayerSpawn :: !(V2 Double)
  , ldGoal        :: !(V2 Double)
  , ldItems       :: ![(V2 Double, ItemId)]
  } deriving (Eq, Show)

-- | Directory scanned by 'discoverLevels'.
levelDirectory :: FilePath
levelDirectory = "assets" </> "levels"

-- | Item marker table: level-file character → item id. Ids are validated
--   against the item registry at startup (Main.validateAssets). Extend when
--   a collectible should be placeable directly in level files.
itemMarkers :: [(Char, ItemId)]
itemMarkers =
  [ ('K', ItemId "gold-key")
  , ('H', ItemId "potion-hp-s")
  , ('S', ItemId "sword-rusty")
  , ('B', ItemId "boots-swift")
  ]

-- | Parse a level file's contents. Jagged lines are padded with spaces to the
--   widest line. Fails loudly (Left) when required markers are missing —
--   never silently substitutes defaults.
parseLevel :: String -> String -> Either String LevelData
parseLevel name raw
  | null nonEmptyRows = Left (name <> ": level file is empty")
  | otherwise = do
      playerSpawn <- findUnique 'P'
      goalPos     <- findUnique 'G'
      Right LevelData
        { ldName        = name
        , ldTilemap     = tilemap
        , ldPlayerSpawn = playerSpawn
        , ldGoal        = goalPos
        , ldItems       = items
        }
  where
    -- Drop trailing blank lines but keep interior ones (they are level rows).
    rows = lines raw
    nonEmptyRows = reverse (dropWhile (all isSpace) (reverse rows))

    h = length nonEmptyRows
    w = maximum (map length nonEmptyRows)
    padded = map (\line -> line ++ replicate (w - length line) ' ') nonEmptyRows

    coords = [ (col, row, char)
             | (row, line) <- zip [0 :: Int ..] padded
             , (col, char) <- zip [0 :: Int ..] line
             ]

    tilePos col row = V2 (fromIntegral col * tileSize) (fromIntegral row * tileSize)

    findUnique c =
      case [ tilePos col row | (col, row, ch) <- coords, ch == c ] of
        (p : _) -> Right p
        []      -> Left (name <> ": missing required marker '" <> [c] <> "'")

    items = [ (tilePos col row + V2 8.0 8.0, item)
            | (col, row, ch) <- coords
            , Just item <- [lookup ch itemMarkers]
            ]

    toTile '#' = Solid
    toTile _   = Empty

    tilemap = Tilemap
      { mapWidth  = w
      , mapHeight = h
      , mapTiles  = V.fromList [ toTile ch | (_, _, ch) <- coords ]
      }

-- | Load and parse one level file.
loadLevelFile :: FilePath -> IO (Either String LevelData)
loadLevelFile path = do
  raw <- readFile path
  pure (parseLevel (takeBaseName path) raw)

-- | Find all level files, sorted by name. The sort order IS the level order,
--   which is why files are conventionally named @01-…txt@, @02-…txt@.
discoverLevels :: IO [FilePath]
discoverLevels = do
  entries <- listDirectory levelDirectory
  pure [ levelDirectory </> e | e <- sort entries, ".txt" `isSuffixOf` e ]
