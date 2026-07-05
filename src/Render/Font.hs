-- | A tiny built-in 3x5 pixel font rendered with filled rectangles, so the
--   game can draw text (menus, HUD, stats) without any font-file dependency.
--   Supports A-Z, 0-9, space, colon, dash and period; anything else renders
--   as blank. Input is upper-cased automatically.
module Render.Font
  ( drawText
  , textWidth
  , glyphFor
  ) where

import Control.Monad (forM_, when)
import Data.Char (toUpper)
import Foreign.C.Types (CInt)
import SDL (($=))
import qualified SDL
import Linear (V2(..), V4(..))

-- | Glyphs are 5 rows of 3 cells; @#@ marks a lit cell.
glyphFor :: Char -> [String]
glyphFor c = case toUpper c of
  'A' -> ["###","# #","###","# #","# #"]
  'B' -> ["## ","# #","## ","# #","## "]
  'C' -> ["###","#  ","#  ","#  ","###"]
  'D' -> ["## ","# #","# #","# #","## "]
  'E' -> ["###","#  ","## ","#  ","###"]
  'F' -> ["###","#  ","## ","#  ","#  "]
  'G' -> ["###","#  ","# #","# #","###"]
  'H' -> ["# #","# #","###","# #","# #"]
  'I' -> ["###"," # "," # "," # ","###"]
  'J' -> ["  #","  #","  #","# #","###"]
  'K' -> ["# #","# #","## ","# #","# #"]
  'L' -> ["#  ","#  ","#  ","#  ","###"]
  'M' -> ["# #","###","###","# #","# #"]
  'N' -> ["## ","# #","# #","# #","# #"]
  'O' -> ["###","# #","# #","# #","###"]
  'P' -> ["###","# #","###","#  ","#  "]
  'Q' -> ["###","# #","# #","###","  #"]
  'R' -> ["###","# #","## ","# #","# #"]
  'S' -> ["###","#  ","###","  #","###"]
  'T' -> ["###"," # "," # "," # "," # "]
  'U' -> ["# #","# #","# #","# #","###"]
  'V' -> ["# #","# #","# #","# #"," # "]
  'W' -> ["# #","# #","###","###","# #"]
  'X' -> ["# #","# #"," # ","# #","# #"]
  'Y' -> ["# #","# #"," # "," # "," # "]
  'Z' -> ["###","  #"," # ","#  ","###"]
  '0' -> ["###","# #","# #","# #","###"]
  '1' -> [" # ","## "," # "," # ","###"]
  '2' -> ["###","  #","###","#  ","###"]
  '3' -> ["###","  #","###","  #","###"]
  '4' -> ["# #","# #","###","  #","  #"]
  '5' -> ["###","#  ","###","  #","###"]
  '6' -> ["###","#  ","###","# #","###"]
  '7' -> ["###","  #","  #","  #","  #"]
  '8' -> ["###","# #","###","# #","###"]
  '9' -> ["###","# #","###","  #","###"]
  ':' -> ["   "," # ","   "," # ","   "]
  '-' -> ["   ","   ","###","   ","   "]
  '.' -> ["   ","   ","   ","   "," # "]
  _   -> ["   ","   ","   ","   ","   "]

-- | Pixel width of a string at the given cell scale (glyph 3 wide + 1 gap).
textWidth :: Double -> String -> Double
textWidth scale s = fromIntegral (length s) * 4.0 * scale - scale

-- | Draw a string at a top-left position. @scale@ is the size of one font
--   cell in screen pixels.
drawText :: SDL.Renderer -> V4 Int -> Double -> V2 Double -> String -> IO ()
drawText renderer (V4 r g b a) scale (V2 x0 y0) str = do
  SDL.rendererDrawColor renderer $=
    V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
  forM_ (zip [0 ..] str) $ \(ci, ch) -> do
    let gx = x0 + fromIntegral (ci :: Int) * 4.0 * scale
    forM_ (zip [0 ..] (glyphFor ch)) $ \(ri, row) ->
      forM_ (zip [0 ..] row) $ \(pi', cell) ->
        when (cell == '#') $ do
          let px = gx + fromIntegral (pi' :: Int) * scale
              py = y0 + fromIntegral (ri :: Int) * scale
              rect = SDL.Rectangle
                       (SDL.P (V2 (round px) (round py)))
                       (V2 (ceilingC scale) (ceilingC scale))
          SDL.fillRect renderer (Just rect)
  where
    ceilingC :: Double -> CInt
    ceilingC = ceiling
