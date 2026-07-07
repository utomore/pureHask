-- | Audio definitions: compiling @assets/audio/audio.def@ and the pure
--   "what should be heard" rules. Playback itself lives in "Audio.Player"
--   (the only module that touches SDL_mixer); this module never does IO
--   beyond reading the definition file.
--
--   Format:
--
--   > (sfx pickup (file sfx/pickup.wav) (volume 96))
--   > (music cave (file music/cave.wav) (volume 64))
--   > (on-event item-picked pickup)      ; GameEvent -> sfx
--   > (music-for-mode title title-theme) ; GameMode  -> music
--   > (music-for-level 03-hollow cave)   ; overrides "playing" per level
--
--   Sounds are DRIVEN BY EVENTS: the simulation publishes 'GameEvent's and
--   the shell plays whatever this table binds to them — simulation and
--   logic code never know audio exists (ARCHITECTURE.md §3.8). Event and
--   mode names are a fixed vocabulary (see 'eventKey' / 'modeKey');
--   binding an unknown name is a startup error. Modes with no binding
--   simply keep the current track playing (that is why the menu does not
--   restart the music).
module Audio.Script
  ( SfxDef(..)
  , MusicDef(..)
  , AudioDefs(..)
  , emptyAudioDefs
  , compileAudio
  , loadAudioFile
  , eventKey
  , sfxForEvent
  , musicFor
  , audioLevelRefs
  ) where

import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import Core.Types (GameEvent(..), GameMode(..))
import Script.Sexp

-- | One sound effect (volume 0-128).
data SfxDef = SfxDef
  { sfId     :: !Text
  , sfFile   :: !FilePath  -- ^ relative to assets/audio/
  , sfVolume :: !Int
  } deriving (Eq, Show)

-- | One music track (volume 0-128).
data MusicDef = MusicDef
  { muId     :: !Text
  , muFile   :: !FilePath
  , muVolume :: !Int
  } deriving (Eq, Show)

-- | The whole audio table.
data AudioDefs = AudioDefs
  { adSfx        :: !(Map Text SfxDef)
  , adMusic      :: !(Map Text MusicDef)
  , adEventSfx   :: !(Map Text Text)  -- ^ event key -> sfx id
  , adModeMusic  :: !(Map Text Text)  -- ^ mode key -> music id
  , adLevelMusic :: !(Map Text Text)  -- ^ level name -> music id
  } deriving (Eq, Show)

emptyAudioDefs :: AudioDefs
emptyAudioDefs = AudioDefs M.empty M.empty M.empty M.empty M.empty

-- | The event vocabulary scripts may bind sounds to. 'Nothing' for events
--   that make no sense as a sound trigger (a restored save is not a noise).
eventKey :: GameEvent -> Maybe Text
eventKey ev = case ev of
  EvPlayerDied      -> Just "player-died"
  EvGoalReached     -> Just "goal-reached"
  EvItemPicked _    -> Just "item-picked"
  EvItemUsed _      -> Just "item-used"
  EvEquipChanged    -> Just "equip-changed"
  EvTalkedTo _      -> Just "talked-to"
  EvEnemyKilled _   -> Just "enemy-killed"
  EvTalentLearned _ -> Just "talent-learned"
  EvTalentsRespec   -> Just "talents-respec"
  EvRunRestored {}  -> Nothing

knownEventKeys :: [Text]
knownEventKeys =
  [ "player-died", "goal-reached", "item-picked", "item-used"
  , "equip-changed", "talked-to", "enemy-killed", "talent-learned"
  , "talents-respec"
  ]

-- | The mode vocabulary for @music-for-mode@.
modeKey :: GameMode -> Text
modeKey mode = case mode of
  ModeTitle           -> "title"
  ModePlaying         -> "playing"
  ModeMenu            -> "menu"
  ModeDialogue _      -> "dialogue"
  ModeDead _          -> "dead"
  ModeLevelComplete _ -> "level-complete"
  ModeEnding          -> "ending"

knownModeKeys :: [Text]
knownModeKeys =
  [ "title", "playing", "menu", "dialogue", "dead", "level-complete", "ending" ]

-- | The sound effect an event should trigger, if any.
sfxForEvent :: AudioDefs -> GameEvent -> Maybe Text
sfxForEvent defs ev = eventKey ev >>= \k -> M.lookup k (adEventSfx defs)

-- | The music that SHOULD be playing. 'Nothing' means "no opinion — keep
--   whatever plays now", which is what unbound modes (menu, dialogue…)
--   want. While playing, a per-level binding beats the generic one.
musicFor :: AudioDefs -> GameMode -> Text -> Maybe Text
musicFor defs mode levelName = case mode of
  ModePlaying ->
    case M.lookup levelName (adLevelMusic defs) of
      Just m  -> Just m
      Nothing -> M.lookup "playing" (adModeMusic defs)
  m -> M.lookup (modeKey m) (adModeMusic defs)

-- | Level names referenced by @music-for-level@ (startup cross-validation).
audioLevelRefs :: AudioDefs -> [Text]
audioLevelRefs = M.keys . adLevelMusic

--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

-- | Compile the audio table. Duplicate ids, bindings to unknown sfx/music
--   ids and unknown event/mode names are hard errors.
compileAudio :: [Sexp] -> Either String AudioDefs
compileAudio forms = do
  sfxList <- mapM compileSfx (formsNamed "sfx" forms)
  musicList <- mapM compileMusic (formsNamed "music" forms)
  sfxMap <- uniqueMap "sfx" [ (sfId s, s) | s <- sfxList ]
  musicMap <- uniqueMap "music" [ (muId m, m) | m <- musicList ]

  eventBinds <- mapM (compileBind "on-event" knownEventKeys (M.keysSet sfxMap))
                  (formsNamed "on-event" forms)
  modeBinds <- mapM (compileBind "music-for-mode" knownModeKeys (M.keysSet musicMap))
                 (formsNamed "music-for-mode" forms)
  levelBinds <- mapM (compileLevelBind (M.keysSet musicMap))
                  (formsNamed "music-for-level" forms)

  Right AudioDefs
    { adSfx = sfxMap
    , adMusic = musicMap
    , adEventSfx = M.fromList eventBinds
    , adModeMusic = M.fromList modeBinds
    , adLevelMusic = M.fromList levelBinds
    }
  where
    uniqueMap what kvs =
      let dups = [ k | (k, n) <- M.toList (M.fromListWith (+) [ (k, 1 :: Int) | (k, _) <- kvs ]), n > 1 ]
      in case dups of
           (d : _) -> Left ("audio.def: duplicate " <> what <> " id '" <> T.unpack d <> "'")
           []      -> Right (M.fromList kvs)

compileSfx :: [Sexp] -> Either String SfxDef
compileSfx body = do
  (rawId, file, vol) <- compileAsset "sfx" body
  Right (SfxDef rawId file vol)

compileMusic :: [Sexp] -> Either String MusicDef
compileMusic body = do
  (rawId, file, vol) <- compileAsset "music" body
  Right (MusicDef rawId file vol)

compileAsset :: String -> [Sexp] -> Either String (Text, FilePath, Int)
compileAsset what [] = Left ("audio.def: (" <> what <> " …) without an id")
compileAsset what (idForm : body) = do
  rawId <- maybe (Left ("audio.def: " <> what <> " id must be a symbol")) Right
             (sexpSymbol idForm)
  let ctx = what <> " '" <> T.unpack rawId <> "'"
  file <- case fieldOf "file" body of
    Just [SSym f] -> Right (T.unpack f)
    Just [SStr f] -> Right (T.unpack f)
    Just other    -> Left (ctx <> ": bad file " <> show other)
    Nothing       -> Left (ctx <> ": missing (file PATH)")
  vol <- case fieldOf "volume" body of
    Just [SNum v] | v >= 0 && v <= 128 -> Right (round v)
    Nothing -> Right 96
    Just other -> Left (ctx <> ": bad volume (0-128) " <> show other)
  Right (rawId, file, vol)

compileBind :: String -> [Text] -> Set Text -> [Sexp] -> Either String (Text, Text)
compileBind what knownKeys knownIds body = case body of
  [SSym key, SSym target]
    | key `notElem` knownKeys ->
        Left ("audio.def: (" <> what <> " …): unknown name '" <> T.unpack key
              <> "' (known: " <> T.unpack (T.intercalate ", " knownKeys) <> ")")
    | not (target `Set.member` knownIds) ->
        Left ("audio.def: (" <> what <> " " <> T.unpack key
              <> " …): unknown id '" <> T.unpack target <> "'")
    | otherwise -> Right (key, target)
  other -> Left ("audio.def: bad (" <> what <> " …) " <> show other)

compileLevelBind :: Set Text -> [Sexp] -> Either String (Text, Text)
compileLevelBind knownIds body = case body of
  [SSym level, SSym target]
    | target `Set.member` knownIds -> Right (level, target)
    | otherwise ->
        Left ("audio.def: (music-for-level " <> T.unpack level
              <> " …): unknown music id '" <> T.unpack target <> "'")
  other -> Left ("audio.def: bad (music-for-level …) " <> show other)

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

-- | Load and compile @audio.def@, then check every referenced file exists
--   under the audio directory. A missing definition file means "silence".
loadAudioFile :: FilePath -> IO (Either String AudioDefs)
loadAudioFile audioDir = do
  let path = audioDir </> "audio.def"
  exists <- doesFileExist path
  if not exists
    then pure (Right emptyAudioDefs)
    else do
      bytes <- BS.readFile path
      case decodeUtf8' bytes of
        Left err -> pure (Left (path <> ": not valid UTF-8 (" <> show err <> ")"))
        Right txt ->
          case parseSexps (stripBom txt) >>= compileAudio of
            Left err -> pure (Left (path <> ": " <> err))
            Right defs -> do
              missing <- missingFiles defs
              pure $ case missing of
                (f : _) -> Left ("audio.def: file " <> f <> " does not exist")
                []      -> Right defs
  where
    stripBom t = maybe t id (T.stripPrefix "\65279" t)
    missingFiles defs = do
      let files = [ sfFile s | s <- M.elems (adSfx defs) ]
               <> [ muFile m | m <- M.elems (adMusic defs) ]
      checked <- mapM (\f -> do
                          ok <- doesFileExist (audioDir </> f)
                          pure (f, ok))
                   files
      pure [ audioDir </> f | (f, ok) <- checked, not ok ]
