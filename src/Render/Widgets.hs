-- | Shared drawing primitives for HUD and menu pages: panels, bars, labelled
--   rows. All geometry-and-fillRect, consistent with the game's minimal
--   aesthetic; text comes from "Render.Font".
module Render.Widgets
  ( Color
  , drawPanel
  , drawBar
  , drawLabelledBar
  ) where

import SDL (($=))
import qualified SDL
import Linear (V2(..), V4(..))

import Render.Draw (toSDLRect)
import Render.Font (FontSet(..), drawText)

type Color = V4 Int

setColor :: SDL.Renderer -> Color -> IO ()
setColor renderer (V4 r g b a) =
  SDL.rendererDrawColor renderer $=
    V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)

-- | A filled, bordered rectangle panel.
drawPanel :: SDL.Renderer -> V2 Double -> V2 Double -> Color -> Color -> IO ()
drawPanel renderer pos size fill border = do
  let rect = toSDLRect pos size
  setColor renderer fill
  SDL.fillRect renderer (Just rect)
  setColor renderer border
  SDL.drawRect renderer (Just rect)

-- | A progress bar: dark trough, proportional fill, border.
drawBar :: SDL.Renderer -> V2 Double -> V2 Double -> Double -> Color -> IO ()
drawBar renderer pos size@(V2 w h) fraction fillColor = do
  let frac = max 0.0 (min 1.0 fraction)
  setColor renderer (V4 12 16 26 230)
  SDL.fillRect renderer (Just (toSDLRect pos size))
  setColor renderer fillColor
  SDL.fillRect renderer (Just (toSDLRect pos (V2 (w * frac) h)))
  setColor renderer (V4 58 74 102 255)
  SDL.drawRect renderer (Just (toSDLRect pos size))

-- | A bar with a small label to its left (e.g. @HP@).
drawLabelledBar :: FontSet -> V2 Double -> V2 Double -> Double
                -> Color -> String -> IO ()
drawLabelledBar fonts (V2 x y) size fraction fillColor label = do
  drawText fonts (V4 170 190 215 255) 1.5 (V2 x (y + 1.0)) label
  drawBar (fsRenderer fonts) (V2 (x + 24.0) y) size fraction fillColor
