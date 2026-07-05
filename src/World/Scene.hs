-- | Scene decoration: the six parallax layers around the main play layer
--   (three behind, three in front), loaded from @assets/scenes/NAME.scene@ —
--   the sidecar of the level file with the same base name. A missing scene
--   file simply means an undecorated level.
--
--   Format:
--
--   > (scene
--   >   (layer back-3 (parallax 0.2)
--   >     (prop silhouette-hills 0 260 640 200))
--   >   (layer front-1 (parallax 1.2) (alpha 180)
--   >     (prop fog-band 0 480 2000 60)))
--
--   Props are named rectangles (pixels, world coordinates); the renderer maps
--   names to colours ("Render.Layers"). Swapping the geometric look for real
--   art later only changes that mapping — scene files stay untouched.
module World.Scene
  ( LayerSlot(..)
  , backSlots
  , frontSlots
  , Prop(..)
  , SceneLayer(..)
  , SceneDef(..)
  , emptyScene
  , layerOf
  , compileScene
  , loadSceneFile
  ) where

import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Linear (V2(..))
import System.Directory (doesFileExist)

import Script.Sexp

-- | The six decoration layers, in draw order relative to the main layer.
data LayerSlot = Back3 | Back2 | Back1 | Front1 | Front2 | Front3
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Painted back to front, before the main layer.
backSlots :: [LayerSlot]
backSlots = [Back3, Back2, Back1]

-- | Painted back to front, after the main layer.
frontSlots :: [LayerSlot]
frontSlots = [Front1, Front2, Front3]

-- | A named rectangle in world coordinates.
data Prop = Prop
  { propName :: !Text
  , propPos  :: !(V2 Double)
  , propSize :: !(V2 Double)
  } deriving (Eq, Show)

data SceneLayer = SceneLayer
  { slParallax :: !Double  -- ^ camera-offset multiplier (<1 far, >1 near)
  , slAlpha    :: !Int     -- ^ layer opacity 0-255
  , slProps    :: ![Prop]
  } deriving (Eq, Show)

newtype SceneDef = SceneDef (Map LayerSlot SceneLayer)
  deriving (Eq, Show)

emptyScene :: SceneDef
emptyScene = SceneDef M.empty

layerOf :: SceneDef -> LayerSlot -> Maybe SceneLayer
layerOf (SceneDef m) slot = M.lookup slot m

slotNames :: [(Text, LayerSlot)]
slotNames =
  [ ("back-3", Back3), ("back-2", Back2), ("back-1", Back1)
  , ("front-1", Front1), ("front-2", Front2), ("front-3", Front3)
  ]

compileScene :: [Sexp] -> Either String SceneDef
compileScene forms = case formsNamed "scene" forms of
  []          -> Right emptyScene
  (body : _)  -> do
    layers <- mapM compileLayer (formsNamed "layer" body)
    Right (SceneDef (M.fromList layers))

compileLayer :: [Sexp] -> Either String (LayerSlot, SceneLayer)
compileLayer [] = Left "scene: (layer …) without a slot name"
compileLayer (slotForm : body) = do
  slotName <- maybe (Left "scene: layer slot must be a symbol") Right
                (sexpSymbol slotForm)
  slot <- maybe (Left ("scene: unknown layer '" <> T.unpack slotName
                       <> "' (use back-3..front-3)"))
                Right
                (lookup slotName slotNames)
  let ctx = "layer '" <> T.unpack slotName <> "'"

  parallax <- case fieldOf "parallax" body of
    Just [SNum p] -> Right p
    Nothing       -> Right (defaultParallax slot)
    Just other    -> Left (ctx <> ": bad parallax " <> show other)

  alpha <- case fieldOf "alpha" body of
    Just [SNum a] -> Right (round a)
    Nothing       -> Right 255
    Just other    -> Left (ctx <> ": bad alpha " <> show other)

  props <- mapM (compileProp ctx) (formsNamed "prop" body)
  Right (slot, SceneLayer parallax alpha props)

-- | Sensible depth defaults when a layer omits @(parallax …)@.
defaultParallax :: LayerSlot -> Double
defaultParallax slot = case slot of
  Back3  -> 0.2
  Back2  -> 0.5
  Back1  -> 0.75
  Front1 -> 1.15
  Front2 -> 1.35
  Front3 -> 1.6

compileProp :: String -> [Sexp] -> Either String Prop
compileProp ctx parts = case parts of
  [SSym name, SNum x, SNum y, SNum w, SNum h] ->
    Right (Prop name (V2 x y) (V2 w h))
  other -> Left (ctx <> ": bad prop " <> show other
                 <> " (expected (prop NAME X Y W H))")

-- | Load a scene file; a missing file yields the empty scene.
loadSceneFile :: FilePath -> IO (Either String SceneDef)
loadSceneFile path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right emptyScene)
    else do
      bytes <- BS.readFile path
      pure $ case decodeUtf8' bytes of
        Left err  -> Left (path <> ": not valid UTF-8 (" <> show err <> ")")
        Right txt ->
          either (Left . ((path <> ": ") <>)) Right
            (parseSexps txt >>= compileScene)
