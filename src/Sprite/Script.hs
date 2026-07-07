-- | Sprite-sheet definitions: compiling @assets/sprites/*.sprite@ files and
--   validating them against the referenced sheet images
--   (@assets/textures/*.bmp@) WITHOUT any SDL dependency — the BMP header is
--   parsed by hand so @--validate@ can check everything headless.
--
--   Format:
--
--   > (sprite player
--   >   (sheet player.bmp)          ; image in assets/textures/ (BMP)
--   >   (frame-size 24 24)          ; grid cell in pixels
--   >   (color-key 255 0 255)       ; optional transparent colour
--   >   (anim idle (row 0) (frames 2) (fps 4))
--   >   (anim run  (row 1) (frames 4) (fps 10)))
--
--   Every animation reads frames left-to-right from ONE row of the grid and
--   loops. Which sprite an entity uses is a naming convention, not a schema
--   field: @player@ for the player, @npc-ID@ for NPCs, @enemy-ID@ for
--   enemies. An entity without a matching sprite keeps its coloured-rect
--   look, so sheets can be added one at a time.
module Sprite.Script
  ( SpriteDef(..)
  , AnimDef(..)
  , compileSprites
  , loadSpriteDir
  , bmpDimensions
  ) where

import qualified Data.ByteString as BS
import Data.List (sort, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Data.Bits (shiftL, (.|.))
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))

import Script.Sexp

-- | One animation: @adFrames@ cells read left-to-right from grid row
--   @adRow@, advancing at @adFps@ frames per second, looping.
data AnimDef = AnimDef
  { adName   :: !Text
  , adRow    :: !Int
  , adFrames :: !Int
  , adFps    :: !Double
  } deriving (Eq, Show)

-- | One sprite sheet and its animations.
data SpriteDef = SpriteDef
  { sdId       :: !Text
  , sdSheet    :: !FilePath              -- ^ file name inside assets/textures/
  , sdFrameW   :: !Int
  , sdFrameH   :: !Int
  , sdColorKey :: !(Maybe (Int, Int, Int))
  , sdAnims    :: ![AnimDef]
  } deriving (Eq, Show)

-- | Compile every @(sprite …)@ form. Duplicate sprite or animation names,
--   malformed fields and non-positive sizes are hard errors.
compileSprites :: [Sexp] -> Either String [SpriteDef]
compileSprites forms = do
  defs <- mapM compileSprite (formsNamed "sprite" forms)
  let ids = map sdId defs
      dups = [ i | (i, n) <- counts ids, n > 1 ]
  case dups of
    (d : _) -> Left ("sprites: duplicate sprite id '" <> T.unpack d <> "'")
    []      -> Right defs
  where
    counts xs = [ (x, length (filter (== x) xs)) | x <- xs ]

compileSprite :: [Sexp] -> Either String SpriteDef
compileSprite [] = Left "sprites: (sprite …) without an id"
compileSprite (idForm : body) = do
  rawId <- maybe (Left "sprites: sprite id must be a symbol") Right
             (sexpSymbol idForm)
  let ctx = "sprite '" <> T.unpack rawId <> "'"

  sheet <- case fieldOf "sheet" body of
    Just [SSym f]  -> Right (T.unpack f)
    Just [SStr f]  -> Right (T.unpack f)
    Just other     -> Left (ctx <> ": bad sheet " <> show other)
    Nothing        -> Left (ctx <> ": missing (sheet FILE.bmp)")

  (fw, fh) <- case fieldOf "frame-size" body of
    Just [SNum w, SNum h] | w >= 1 && h >= 1 -> Right (round w, round h)
    Just other -> Left (ctx <> ": bad frame-size " <> show other)
    Nothing    -> Left (ctx <> ": missing (frame-size W H)")

  colorKey <- case fieldOf "color-key" body of
    Nothing -> Right Nothing
    Just [SNum r, SNum g, SNum b] -> Right (Just (round r, round g, round b))
    Just other -> Left (ctx <> ": bad color-key " <> show other)

  anims <- mapM (compileAnim ctx) (formsNamed "anim" body)
  case anims of
    [] -> Left (ctx <> ": needs at least one (anim …)")
    _  -> do
      let names = map adName anims
      case [ n | n <- names, length (filter (== n) names) > 1 ] of
        (n : _) -> Left (ctx <> ": duplicate anim '" <> T.unpack n <> "'")
        []      -> Right SpriteDef
          { sdId = rawId, sdSheet = sheet
          , sdFrameW = fw, sdFrameH = fh
          , sdColorKey = colorKey, sdAnims = anims
          }

compileAnim :: String -> [Sexp] -> Either String AnimDef
compileAnim ctx [] = Left (ctx <> ": (anim …) without a name")
compileAnim ctx (nameForm : parts) = do
  name <- maybe (Left (ctx <> ": anim name must be a symbol")) Right
            (sexpSymbol nameForm)
  let actx = ctx <> " anim '" <> T.unpack name <> "'"
  row <- case fieldOf "row" parts of
    Just [SNum r] | r >= 0 -> Right (round r)
    Nothing                -> Right 0
    Just other             -> Left (actx <> ": bad row " <> show other)
  frames <- case fieldOf "frames" parts of
    Just [SNum n] | n >= 1 -> Right (round n)
    Nothing                -> Right 1
    Just other             -> Left (actx <> ": bad frames " <> show other)
  fps <- case fieldOf "fps" parts of
    Just [SNum f] | f > 0 -> Right f
    Nothing               -> Right 8.0
    Just other            -> Left (actx <> ": bad fps " <> show other)
  Right (AnimDef name row frames fps)

--------------------------------------------------------------------------------
-- BMP header check (pure)
--------------------------------------------------------------------------------

-- | Width and height from a BMP file's header (BITMAPINFOHEADER family).
--   Enough to validate frame grids without an image library.
bmpDimensions :: BS.ByteString -> Either String (Int, Int)
bmpDimensions bytes
  | BS.length bytes < 26 = Left "not a BMP file (too short)"
  | BS.take 2 bytes /= BS.pack [0x42, 0x4D] = Left "not a BMP file (bad magic)"
  | otherwise =
      let le32 off = fromIntegral (BS.index bytes off)
                 .|. (fromIntegral (BS.index bytes (off + 1)) `shiftL` 8)
                 .|. (fromIntegral (BS.index bytes (off + 2)) `shiftL` 16)
                 .|. (fromIntegral (BS.index bytes (off + 3)) `shiftL` 24) :: Int
          w = le32 18
          h = le32 22
      in if w < 1 || h < 1 || h > 0x7FFFFFFF - 1
           then Left "BMP with non-positive dimensions (top-down BMPs store a negative height; export bottom-up)"
           else Right (w, h)

-- | Check every animation of a sprite fits inside its sheet image.
checkAgainstSheet :: FilePath -> SpriteDef -> IO (Either String ())
checkAgainstSheet textureDir def = do
  let path = textureDir </> sdSheet def
      ctx = "sprite '" <> T.unpack (sdId def) <> "'"
  exists <- doesFileExist path
  if not exists
    then pure (Left (ctx <> ": sheet " <> path <> " does not exist"))
    else do
      bytes <- BS.readFile path
      pure $ do
        (w, h) <- either (Left . ((ctx <> ": " <> path <> ": ") <>)) Right
                    (bmpDimensions bytes)
        mapM_ (fits ctx w h) (sdAnims def)
  where
    fits ctx w h anim
      | adFrames anim * sdFrameW def > w =
          Left (ctx <> " anim '" <> T.unpack (adName anim)
                <> "': " <> show (adFrames anim) <> " frames of width "
                <> show (sdFrameW def) <> " exceed the sheet width " <> show w)
      | (adRow anim + 1) * sdFrameH def > h =
          Left (ctx <> " anim '" <> T.unpack (adName anim)
                <> "': row " <> show (adRow anim) <> " exceeds the sheet height "
                <> show h)
      | otherwise = Right ()

-- | Load and compile every @*.sprite@ file in a directory (sorted), then
--   validate each sheet reference against the real image on disk. A missing
--   directory means "no sprites" — everything keeps its rectangle look.
loadSpriteDir :: FilePath -> FilePath -> IO (Either String [SpriteDef])
loadSpriteDir dir textureDir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure (Right [])
    else do
      entries <- listDirectory dir
      let files = [ dir </> e | e <- sort entries, ".sprite" `isSuffixOf` e ]
      results <- mapM loadOne files
      case sequence results >>= compileSprites . concat of
        Left err -> pure (Left err)
        Right defs -> do
          checks <- mapM (checkAgainstSheet textureDir) defs
          pure (sequence_ checks >> Right defs)
  where
    loadOne path = do
      bytes <- BS.readFile path
      pure $ case decodeUtf8' bytes of
        Left err -> Left (path <> ": not valid UTF-8 (" <> show err <> ")")
        Right txt ->
          either (Left . ((path <> ": ") <>)) Right (parseSexps (stripBom txt))
    stripBom t = maybe t id (T.stripPrefix "\65279" t)
