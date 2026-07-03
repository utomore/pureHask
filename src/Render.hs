module Render where

import Control.Monad (when, forM_)
import Foreign.C.Types (CInt)
import Apecs hiding (($=))
import SDL (($=))
import qualified SDL
import Linear (V2(..), V4(..))

import Types
import Map

-- | Helper to convert game coordinates (Double) to SDL CInt Rectangle.
toSDLRect :: V2 Double -> V2 Double -> SDL.Rectangle CInt
toSDLRect (V2 x y) (V2 w h) =
  let rx = round x
      ry = round y
      rw = round w
      rh = round h
  in SDL.Rectangle (SDL.P (V2 rx ry)) (V2 rw rh)

-- | Main rendering loop for the game scene.
renderGame :: SDL.Renderer -> Tilemap -> Game ()
renderGame renderer tilemap = do
  -- 1. Query player position for camera centering
  playerPositions <- cfold (\acc (Player, Position p) -> p : acc) []
  let playerPos = case playerPositions of
        (p:_) -> p
        []    -> V2 0.0 0.0
        
  -- Screen dimensions
  let screenW = 800.0
      screenH = 600.0
      V2 px py = playerPos
      
  -- 2. Center camera on player, clamped within the map boundaries
  let mapW = fromIntegral (mapWidth tilemap) * tileSize
      mapH = fromIntegral (mapHeight tilemap) * tileSize
      camX = max 0.0 (min (mapW - screenW) (px - screenW / 2.0))
      camY = max 0.0 (min (mapH - screenH) (py - screenH / 2.0))
      camOffset = V2 camX camY

  -- 3. Clear Screen with a sleek dark-mode background (#121826)
  SDL.rendererDrawColor renderer $= V4 18 24 38 255
  SDL.clear renderer

  -- 4. Draw Tilemap (render only visible tiles for optimization)
  let startCol = max 0 (floor (camX / tileSize))
      endCol = min (mapWidth tilemap - 1) (ceiling ((camX + screenW) / tileSize))
      startRow = max 0 (floor (camY / tileSize))
      endRow = min (mapHeight tilemap - 1) (ceiling ((camY + screenH) / tileSize))

  forM_ [startCol .. endCol] $ \col ->
    forM_ [startRow .. endRow] $ \row -> do
      let isSolid = isTileSolid tilemap col row
      when isSolid $ do
        let tx = fromIntegral col * tileSize - camX
            ty = fromIntegral row * tileSize - camY
            rect = toSDLRect (V2 tx ty) (V2 tileSize tileSize)
            
        -- Fill: Slate-gray/blue tile color (#2B374D)
        SDL.rendererDrawColor renderer $= V4 43 55 77 255
        SDL.fillRect renderer (Just rect)
        
        -- Border: Slightly lighter border (#3A4A66) for grid definition
        SDL.rendererDrawColor renderer $= V4 58 74 102 255
        SDL.drawRect renderer (Just rect)

  -- 5. Draw Goal Entity (Neon Violet/Magenta #DC32B4)
  cfoldM_ (\_ (Goal, Position pos, Collider size) -> do
    let relativePos = pos - camOffset
        rect = toSDLRect relativePos size
    SDL.rendererDrawColor renderer $= V4 220 50 180 255
    SDL.fillRect renderer (Just rect)
    SDL.rendererDrawColor renderer $= V4 255 100 220 255
    SDL.drawRect renderer (Just rect)
    ) ()

  -- 6. Draw Player Entity (Vibrant Neon Cyan/Teal #00DCDC)
  cfoldM_ (\_ (Player, Position pos, Collider size) -> do
    let relativePos = pos - camOffset
        rect = toSDLRect relativePos size
    SDL.rendererDrawColor renderer $= V4 0 220 220 255
    SDL.fillRect renderer (Just rect)
    SDL.rendererDrawColor renderer $= V4 100 255 255 255
    SDL.drawRect renderer (Just rect)
    ) ()

  -- 7. Swap buffers to display the rendered frame
  SDL.present renderer
