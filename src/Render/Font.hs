-- | Text rendering with two backends, selected at runtime by the FONT row in
--   the settings menu (see 'Core.Settings.FontChoice'):
--
--     * 'FontPixel': the built-in 3x5 pixel font drawn with filled
--       rectangles — ASCII only. Strings containing non-ASCII characters
--       (Chinese dialogue, item names…) automatically fall back to the TTF
--       backend, so switching to the pixel style never makes text vanish.
--     * 'FontChinese': a bundled Traditional-Chinese TrueType font rendered
--       through SDL2_ttf (full CJK + Latin coverage).
--
--   Crispness rule: TTF text is rasterised AT THE SIZE IT IS DRAWN (one
--   cached font per point size) and blitted 1:1. Rasterising one big master
--   size and scaling down destroys thin strokes — that bug shipped once;
--   don't reintroduce it.
--
--   Every draw call goes through a per-frame 'FontSet' built by the shell,
--   so toggling the setting takes effect immediately. Because TTF glyph
--   widths must be measured against shaped glyphs, 'textWidth' is IO.
module Render.Font
  ( Fonts
  , FontSet(..)
  , loadFonts
  , freeFonts
  , drawText
  , textWidth
  , glyphFor
  ) where

import Control.Monad (forM_, unless, when)
import Data.Char (toUpper, isAscii)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF
import System.Directory (doesFileExist)
import Linear (V2(..), V4(..))

import Core.Settings (FontChoice(..))

-- | The TTF font source and its per-point-size cache. Sizes are loaded on
--   demand (the UI uses fewer than ten distinct sizes) and freed together.
data Fonts = Fonts
  { fontsPath  :: !FilePath
  , fontsCache :: !(IORef (M.Map Int TTF.Font))
  }

-- | Everything a frame's text is drawn with: the renderer, the active
--   choice from the settings, and the loaded TTF fonts.
data FontSet = FontSet
  { fsRenderer :: !SDL.Renderer
  , fsChoice   :: !FontChoice
  , fsFonts    :: !Fonts
  }

-- | Initialise SDL2_ttf and prepare the Traditional-Chinese font. A missing
--   or unloadable font file aborts the launch with a clear message.
loadFonts :: FilePath -> IO Fonts
loadFonts path = do
  TTF.initialize
  exists <- doesFileExist path
  unless exists $
    fail ("font load failed: " <> path <> " not found (run from the project root)")
  probe <- TTF.load path 16  -- fail loudly NOW if the file is not a font
  TTF.free probe
  Fonts path <$> newIORef M.empty

freeFonts :: Fonts -> IO ()
freeFonts fonts = do
  cache <- readIORef (fontsCache fonts)
  mapM_ TTF.free (M.elems cache)
  writeIORef (fontsCache fonts) M.empty
  TTF.quit

--------------------------------------------------------------------------------
-- Shared metrics and dispatch
--------------------------------------------------------------------------------

-- | Point size for a given pixel-font cell scale. Chosen so the TTF cap
--   height roughly matches the 5-cell pixel glyph body.
pointFor :: Double -> Int
pointFor scale = max 8 (round (6.0 * scale))

-- | The cached font for a point size, loading it on first use.
fontAt :: Fonts -> Int -> IO TTF.Font
fontAt fonts pt = do
  cache <- readIORef (fontsCache fonts)
  case M.lookup pt cache of
    Just f  -> pure f
    Nothing -> do
      f <- TTF.load (fontsPath fonts) pt
      modifyIORef' (fontsCache fonts) (M.insert pt f)
      pure f

-- | Which backend renders this string? The pixel font covers ASCII only;
--   anything else falls back to the TTF so no text ever disappears.
usesTtf :: FontChoice -> String -> Bool
usesTtf FontChinese _   = True
usesTtf FontPixel   str = any (not . isAscii) str

-- | Pixel width of a string at the given cell scale. IO because the TTF
--   backend has to measure shaped glyphs.
textWidth :: FontSet -> Double -> String -> IO Double
textWidth fs scale str
  | usesTtf (fsChoice fs) str =
      if null str
        then pure 0.0
        else do
          font <- fontAt (fsFonts fs) (pointFor scale)
          (w, _) <- TTF.size font (T.pack str)
          pure (fromIntegral w)
  | otherwise = pure (fromIntegral (length str) * 4.0 * scale - scale)

-- | Draw a string at a top-left position. @scale@ is the size of one font
--   cell in screen pixels (pixel-font metric; the TTF backend matches it).
drawText :: FontSet -> V4 Int -> Double -> V2 Double -> String -> IO ()
drawText fs color scale pos str
  | usesTtf (fsChoice fs) str =
      drawTtfText (fsRenderer fs) (fsFonts fs) color scale pos str
  | otherwise = drawPixelText (fsRenderer fs) color scale pos str

--------------------------------------------------------------------------------
-- TTF backend
--------------------------------------------------------------------------------

-- | Render a string through SDL2_ttf at its native size (no scaling):
--   blended surface -> texture -> 1:1 copy. Textures are per-call; UI text
--   volume is small enough that no glyph cache is needed.
drawTtfText :: SDL.Renderer -> Fonts -> V4 Int -> Double -> V2 Double
            -> String -> IO ()
drawTtfText renderer fonts (V4 r g b a) scale (V2 x y) str =
  unless (null str) $ do
    font <- fontAt fonts (pointFor scale)
    surface <- TTF.blended font (V4 (chan r) (chan g) (chan b) 255) (T.pack str)
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface
    SDL.textureBlendMode texture $= SDL.BlendAlphaBlend
    SDL.textureAlphaMod texture $= chan a
    info <- SDL.queryTexture texture
    let tw = SDL.textureWidth info
        th = SDL.textureHeight info
        -- Center the TTF line box on the 5-cell pixel glyph body so both
        -- backends share a visual baseline area.
        yOff = (fromIntegral th - 5.0 * scale) / 2.0
        dest = SDL.Rectangle (SDL.P (V2 (round x) (round (y - yOff)))) (V2 tw th)
    SDL.copy renderer texture Nothing (Just dest)
    SDL.destroyTexture texture
  where
    chan :: Int -> Word8
    chan = fromIntegral . max 0 . min 255

--------------------------------------------------------------------------------
-- Pixel backend (the original zero-dependency 3x5 font)
--------------------------------------------------------------------------------

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

drawPixelText :: SDL.Renderer -> V4 Int -> Double -> V2 Double -> String -> IO ()
drawPixelText renderer (V4 r g b a) scale (V2 x0 y0) str = do
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
