-- | Sprite-sheet glue: turning validated 'SpriteDef's into SDL textures at
--   startup (and again after a hot reload) and blitting the frame that
--   "Sprite.Core" picks. No decisions live here.
module Render.Sprites
  ( SpriteTextures
  , loadSpriteTextures
  , destroySpriteTextures
  , drawSprite
  ) where

import Control.Exception (try, SomeException)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import SDL (($=))
import qualified SDL
import Linear (V2(..), V4(..))
import System.FilePath ((</>))

import Sprite.Core
import Sprite.Script

-- | Sprite id -> ready-to-blit texture.
type SpriteTextures = M.Map Text SDL.Texture

-- | Load every sprite's sheet into a texture. BMP only (plain SDL, no extra
--   native library); the optional colour key marks transparency. A sheet
--   that fails to load is a hard error — definitions were already validated,
--   so this only fires on a truly broken image file.
loadSpriteTextures :: SDL.Renderer -> FilePath -> [SpriteDef]
                   -> IO (Either String SpriteTextures)
loadSpriteTextures renderer textureDir defs = go M.empty defs
  where
    go acc [] = pure (Right acc)
    go acc (def : rest) = do
      result <- try (loadOne def) :: IO (Either SomeException SDL.Texture)
      case result of
        Left err -> pure (Left ("sprite '" <> T.unpack (sdId def) <> "': "
                                <> show err))
        Right tex -> go (M.insert (sdId def) tex acc) rest

    loadOne def = do
      surface <- SDL.loadBMP (textureDir </> sdSheet def)
      case sdColorKey def of
        Nothing -> pure ()
        Just (r, g, b) ->
          SDL.surfaceColorKey surface $= Just (V4 (chan r) (chan g) (chan b) 255)
      tex <- SDL.createTextureFromSurface renderer surface
      SDL.freeSurface surface
      pure tex

    chan :: Int -> Word8
    chan = fromIntegral . max 0 . min 255

-- | Free every texture (before swapping in a reloaded cache).
destroySpriteTextures :: SpriteTextures -> IO ()
destroySpriteTextures = mapM_ SDL.destroyTexture . M.elems

-- | Blit one entity's sprite: pick an animation by preference, pick the
--   frame from the clock, stretch it over the destination rectangle.
--   Returns 'False' when the sprite (or its texture) does not exist so the
--   caller can fall back to its coloured rectangle.
drawSprite :: SDL.Renderer -> M.Map Text SpriteDef -> SpriteTextures
           -> Text          -- ^ sprite id (naming convention)
           -> [Text]        -- ^ animation preference list
           -> Double        -- ^ animation clock (seconds)
           -> V2 Double     -- ^ destination top-left (screen pixels)
           -> V2 Double     -- ^ destination size
           -> Bool          -- ^ flip horizontally (facing left)
           -> IO Bool
drawSprite renderer defs texs sid animPrefs t pos size flipX =
  case (M.lookup sid defs, M.lookup sid texs) of
    (Just def, Just tex)
      | Just anim <- animOrFallback def animPrefs -> do
          let (sx, sy) = frameOrigin def anim (frameIndex anim t)
              src = SDL.Rectangle
                      (SDL.P (V2 (ci sx) (ci sy)))
                      (V2 (ci (sdFrameW def)) (ci (sdFrameH def)))
              dst = SDL.Rectangle
                      (SDL.P (fmap round pos))
                      (fmap round size)
          SDL.copyEx renderer tex (Just src) (Just dst) 0 Nothing
            (V2 flipX False)
          pure True
    _ -> pure False
  where
    ci :: Int -> CInt
    ci = fromIntegral
