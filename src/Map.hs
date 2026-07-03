module Map where

import qualified Data.Vector as V
import Linear (V2(..))
import Types

-- | Tile types in our level.
data Tile = Empty | Solid deriving (Eq, Show)

-- | Static 2D Grid map representation.
data Tilemap = Tilemap
  { mapWidth  :: !Int
  , mapHeight :: !Int
  , mapTiles  :: !(V.Vector Tile)
  } deriving (Eq, Show)

-- | Hardcoded default layout for the game level.
--   '#' represents a Solid block.
--   ' ' represents an Empty space.
--   'P' represents the Player starting spawn position.
--   'G' represents the Goal position.
defaultMapLayout :: [String]
defaultMapLayout =
  [ "################################################################"
  , "#                                                              #"
  , "#                                                              #"
  , "#                                                              #"
  , "#                                                              #"
  , "#                                                              #"
  , "#             ######                                           #"
  , "#                                                              #"
  , "#                                                              #"
  , "#                                                              #"
  , "#       #####                      ######                      #"
  , "#                                                              #"
  , "#                                                              #"
  , "#                                                 G            #"
  , "#    P               ######                     #####          #"
  , "##################   ######   ##################################"
  , "##################           ###################################"
  , "##################           ###################################"
  , "################################################################"
  ]

-- | Parses the list of strings into a Tilemap and extracts the player
--   spawn position and goal position.
parseTilemap :: [String] -> (Tilemap, V2 Double, V2 Double)
parseTilemap linesList = (tilemap, playerSpawn, goalPos)
  where
    h = length linesList
    -- Find maximum line width to support jagged maps robustly
    w = if h > 0 then maximum (map length linesList) else 0
    
    -- Pad all lines to the maximum width with spaces
    paddedLines = map (\line -> line ++ replicate (w - length line) ' ') linesList
    
    -- Extract information by scanning coords of padded lines
    coords = [ (col, row, char)
             | (row, line) <- zip [0..] paddedLines
             , (col, char) <- zip [0..] line
             ]
             
    findCoord char =
      case filter (\(_, _, c) -> c == char) coords of
        ((col, row, _):_) -> V2 (fromIntegral col * tileSize) (fromIntegral row * tileSize)
        []                -> V2 (100.0) (100.0) -- Fallback default
        
    playerSpawn = findCoord 'P'
    goalPos     = findCoord 'G'
    
    -- Convert characters to Tiles (P and G are parsed as Empty tiles)
    toTile '#' = Solid
    toTile _   = Empty
    
    flatTiles = V.fromList [ toTile char | (_, _, char) <- coords ]
    
    tilemap = Tilemap
      { mapWidth  = w
      , mapHeight = h
      , mapTiles  = flatTiles
      }

-- | Check if a grid position is solid. Out of horizontal bounds is solid,
--   top bounds is empty, bottom bounds is empty (death pit).
isTileSolid :: Tilemap -> Int -> Int -> Bool
isTileSolid tilemap col row
  | col < 0 || col >= mapWidth tilemap = True
  | row < 0 = False
  | row >= mapHeight tilemap = False
  | otherwise =
      let idx = row * mapWidth tilemap + col
      in (mapTiles tilemap V.! idx) == Solid

-- | Returns the grid coordinates (col, row) of all tiles that overlap
--   the given bounding box defined by top-left pos and size.
getOverlappingTiles :: Tilemap -> V2 Double -> V2 Double -> [(Int, Int)]
getOverlappingTiles tilemap (V2 x y) (V2 w h) =
  [ (col, row)
  | col <- [minCol .. maxCol]
  , row <- [minRow .. maxRow]
  ]
  where
    minCol = floor (x / tileSize)
    maxCol = floor ((x + w - 0.001) / tileSize)
    minRow = floor (y / tileSize)
    maxRow = floor ((y + h - 0.001) / tileSize)

-- | Resolves collisions against the Tilemap grid.
--   Performs X-axis movement and resolution, followed by Y-axis movement and resolution.
--   Returns (newPosition, newVelocity, isGrounded).
resolveCollisions :: Tilemap -> V2 Double -> V2 Double -> V2 Double -> Double -> (V2 Double, V2 Double, Bool)
resolveCollisions tilemap pos (V2 vx vy) size dt =
  let
    -- 1. Update X
    posX' = pos + V2 (vx * dt) 0.0
    (posX'', vx') = resolveX tilemap pos posX' size vx
    
    -- 2. Update Y
    posY' = posX'' + V2 0.0 (vy * dt)
    (posY'', vy', grounded) = resolveY tilemap posX'' posY' size vy
  in (posY'', V2 vx' vy', grounded)

resolveX :: Tilemap -> V2 Double -> V2 Double -> V2 Double -> Double -> (V2 Double, Double)
resolveX tilemap _ (V2 newX newY) size@(V2 w _) vx
  | vx == 0 = (V2 newX newY, vx)
  | otherwise =
      let
        overlapping = getOverlappingTiles tilemap (V2 newX newY) size
        solids = filter (\(c, r) -> isTileSolid tilemap c r) overlapping
      -- If there's a collision, snap the position flush with the tile boundary.
      in if null solids
         then (V2 newX newY, vx)
         else if vx > 0
              then let minSolidCol = minimum (map fst solids)
                       snapX = fromIntegral minSolidCol * tileSize - w
                   in (V2 (snapX - 0.001) newY, 0.0)
              else let maxSolidCol = maximum (map fst solids)
                       snapX = fromIntegral (maxSolidCol + 1) * tileSize
                   in (V2 (snapX + 0.001) newY, 0.0)

resolveY :: Tilemap -> V2 Double -> V2 Double -> V2 Double -> Double -> (V2 Double, Double, Bool)
resolveY tilemap _ (V2 newX newY) size@(V2 _ h) vy
  | vy == 0 =
      -- Even if not moving vertically, check if there is ground below.
      let overlapping = getOverlappingTiles tilemap (V2 newX (newY + 1.0)) size
          grounded = any (\(c, r) -> isTileSolid tilemap c r) overlapping
      in (V2 newX newY, vy, grounded)
  | otherwise =
      let
        overlapping = getOverlappingTiles tilemap (V2 newX newY) size
        solids = filter (\(c, r) -> isTileSolid tilemap c r) overlapping
      in if null solids
         then (V2 newX newY, vy, False)
         else if vy > 0
              then let minSolidRow = minimum (map snd solids)
                       snapY = fromIntegral minSolidRow * tileSize - h
                   in (V2 newX (snapY - 0.001), 0.0, True)
              else let maxSolidRow = maximum (map snd solids)
                       snapY = fromIntegral (maxSolidRow + 1) * tileSize
                   in (V2 newX (snapY + 0.001), 0.0, False)
