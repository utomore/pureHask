-- | Drawing the parallax decoration layers. The prop-name → colour palette
--   lives here: swapping the geometric look for textures later means
--   changing only this module; scene files stay untouched.
module Render.Layers
  ( renderLayerSlots
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import SDL (($=))
import qualified SDL
import Linear (V2(..), V4(..))

import Core.Config (screenWidth, screenHeight)
import World.Scene

-- Local rect conversion ("Render.Draw" imports this module, so we cannot
-- import its helper without a cycle).
toRect :: V2 Double -> V2 Double -> SDL.Rectangle CInt
toRect (V2 x y) (V2 w h) =
  SDL.Rectangle (SDL.P (V2 (round x) (round y))) (V2 (round w) (round h))

-- | Palette for named props. Unknown names render in a neutral slate so a
--   typo is visible instead of invisible.
propColor :: Text -> V4 Word8
propColor name = case name of
  "silhouette-hills" -> V4 24 32 52 255
  "silhouette-spire" -> V4 30 38 60 255
  "pillar"           -> V4 34 44 68 255
  "crystal"          -> V4 60 90 140 255
  "glow"             -> V4 70 110 160 90
  "hanging-vine"     -> V4 30 60 48 255
  "fog-band"         -> V4 90 110 140 60
  "dust-band"        -> V4 120 110 90 50
  _                  -> V4 70 80 100 255

-- | Draw the given slots of a scene, applying each layer's parallax factor
--   to the camera offset. Props outside the view are skipped.
renderLayerSlots :: SDL.Renderer -> V2 Double -> SceneDef -> [LayerSlot] -> IO ()
renderLayerSlots renderer camOffset scene slots =
  forM_ slots $ \slot ->
    forM_ (layerOf scene slot) $ \layer -> do
      let offset = camOffset * pure (slParallax layer)
          alpha = fromIntegral (max 0 (min 255 (slAlpha layer))) :: Word8
      forM_ (slProps layer) $ \prop -> do
        let pos@(V2 px py) = propPos prop - offset
            V2 w h = propSize prop
        if px + w < 0 || px > screenWidth || py + h < 0 || py > screenHeight
          then return ()
          else do
            let V4 r g b a = propColor (propName prop)
                a' = fromIntegral (fromIntegral a * fromIntegral alpha `div` (255 :: Int))
            SDL.rendererDrawColor renderer $= V4 r g b a'
            SDL.fillRect renderer (Just (toRect pos (propSize prop)))
