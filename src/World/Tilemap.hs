-- | Static tile grid and AABB-vs-grid collision resolution. Pure — no ECS,
--   no SDL — so every function here is directly unit-testable.
module World.Tilemap
  ( Tile(..)
  , Tilemap(..)
  , mapPixelWidth
  , mapPixelHeight
  , isTileSolid
  , getOverlappingTiles
  , anySolid
  , resolveCollisions
  ) where

import qualified Data.Vector as V
import Linear (V2(..))

import Core.Config (tileSize)

-- | Tile types. Extend this (plus "World.Level" marker parsing and the
--   renderer's tile colours) to add spikes, one-way platforms, etc.
data Tile = Empty | Solid deriving (Eq, Show)

-- | Row-major static grid.
data Tilemap = Tilemap
  { mapWidth  :: !Int
  , mapHeight :: !Int
  , mapTiles  :: !(V.Vector Tile)
  } deriving (Eq, Show)

-- | Map width in pixels.
mapPixelWidth :: Tilemap -> Double
mapPixelWidth tm = fromIntegral (mapWidth tm) * tileSize

-- | Map height in pixels.
mapPixelHeight :: Tilemap -> Double
mapPixelHeight tm = fromIntegral (mapHeight tm) * tileSize

-- | Solidity query with explicit out-of-bounds semantics: beyond the left or
--   right edge counts as solid (walls), above the top is empty (jump space),
--   below the bottom is empty (death pit).
isTileSolid :: Tilemap -> Int -> Int -> Bool
isTileSolid tilemap col row
  | col < 0 || col >= mapWidth tilemap = True
  | row < 0                            = False
  | row >= mapHeight tilemap           = False
  | otherwise =
      let idx = row * mapWidth tilemap + col
      in (mapTiles tilemap V.! idx) == Solid

-- | Grid coordinates (col, row) of all tiles overlapping the AABB given by
--   top-left position and size.
getOverlappingTiles :: Tilemap -> V2 Double -> V2 Double -> [(Int, Int)]
getOverlappingTiles _ (V2 x y) (V2 w h) =
  [ (col, row) | col <- [minCol .. maxCol], row <- [minRow .. maxRow] ]
  where
    minCol = floor (x / tileSize)
    maxCol = floor ((x + w - 0.001) / tileSize)
    minRow = floor (y / tileSize)
    maxRow = floor ((y + h - 0.001) / tileSize)

-- | True when the AABB overlaps at least one solid tile.
anySolid :: Tilemap -> V2 Double -> V2 Double -> Bool
anySolid tilemap pos size =
  any (\(c, r) -> isTileSolid tilemap c r) (getOverlappingTiles tilemap pos size)

-- | Resolve movement against the grid: X axis first, then Y axis, snapping
--   flush to tile boundaries on contact. Returns (newPosition, newVelocity,
--   isGrounded).
resolveCollisions :: Tilemap -> V2 Double -> V2 Double -> V2 Double -> Double
                  -> (V2 Double, V2 Double, Bool)
resolveCollisions tilemap pos (V2 vx vy) size dt =
  let posX = pos + V2 (vx * dt) 0.0
      (posX', vx') = resolveX tilemap posX size vx
      posY = posX' + V2 0.0 (vy * dt)
      (posY', vy', grounded) = resolveY tilemap posY size vy
  in (posY', V2 vx' vy', grounded)

resolveX :: Tilemap -> V2 Double -> V2 Double -> Double -> (V2 Double, Double)
resolveX tilemap p@(V2 _ newY) size@(V2 w _) vx
  | vx == 0 = (p, vx)
  | otherwise =
      let overlapping = getOverlappingTiles tilemap p size
          solids = filter (\(c, r) -> isTileSolid tilemap c r) overlapping
      in if null solids
         then (p, vx)
         else if vx > 0
              then let minSolidCol = minimum (map fst solids)
                       snapX = fromIntegral minSolidCol * tileSize - w
                   in (V2 (snapX - 0.001) newY, 0.0)
              else let maxSolidCol = maximum (map fst solids)
                       snapX = fromIntegral (maxSolidCol + 1) * tileSize
                   in (V2 (snapX + 0.001) newY, 0.0)

resolveY :: Tilemap -> V2 Double -> V2 Double -> Double -> (V2 Double, Double, Bool)
resolveY tilemap p@(V2 newX newY) size@(V2 _ h) vy
  | vy == 0 =
      -- Even when not moving vertically, probe one pixel down for ground.
      let grounded = anySolid tilemap (V2 newX (newY + 1.0)) size
      in (p, vy, grounded)
  | otherwise =
      let overlapping = getOverlappingTiles tilemap p size
          solids = filter (\(c, r) -> isTileSolid tilemap c r) overlapping
      in if null solids
         then (p, vy, False)
         else if vy > 0
              then let minSolidRow = minimum (map snd solids)
                       snapY = fromIntegral minSolidRow * tileSize - h
                   in (V2 newX (snapY - 0.001), 0.0, True)
              else let maxSolidRow = maximum (map snd solids)
                       snapY = fromIntegral (maxSolidRow + 1) * tileSize
                   in (V2 newX (snapY + 0.001), 0.0, False)
