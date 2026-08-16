-- | Audio playback glue — the only module that talks to SDL_mixer. The
--   shell ("Main") calls exactly three things per frame: 'playEventSfx' for
--   the simulation events it just drained, 'syncMusic' with the mode and
--   level the flow machine decided on, and nothing else. WHAT is heard is
--   decided by the pure table in "Audio.Script"; this module only executes.
--
--   A machine without an audio device must still run the game: 'initAudio'
--   returns 'Nothing' on failure and every entry point takes the handle as
--   @Maybe@-wrapped state at the call site.
module Audio.Player
  ( AudioState
  , initAudio
  , loadAudioState
  , destroyAudioState
  , shutdownAudio
  , playEventSfx
  , syncMusic
  ) where

import Control.Exception (try, SomeException)
import Data.IORef
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified SDL.Mixer as Mixer

import Audio.Script
import Core.Types (GameEvent, GameMode)

-- | Loaded chunks and tracks, plus which track currently plays.
data AudioState = AudioState
  { auChunks  :: !(M.Map Text Mixer.Chunk)
  , auMusic   :: !(M.Map Text (Mixer.Music, Int))
  , auCurrent :: !(IORef (Maybe Text))
  }

-- | Open the mixer device (44.1kHz stereo, small buffer for snappy SFX).
--   'Nothing' = no audio on this machine; the game plays silent.
initAudio :: IO (Maybe ())
initAudio = do
  result <- try (Mixer.openAudio Mixer.defaultAudio 1024)
              :: IO (Either SomeException ())
  case result of
    Left err -> do
      putStrLn ("[audio] no audio device, playing silent: " <> show err)
      pure Nothing
    Right () -> pure (Just ())

-- | Load every sound and track in the table (files were validated at
--   startup; a failure here is a broken file, reported not fatal so a hot
--   reload cannot kill a running game).
loadAudioState :: FilePath -> AudioDefs -> IO (Either String AudioState)
loadAudioState audioDir defs = do
  result <- try build :: IO (Either SomeException AudioState)
  pure $ either (Left . show) Right result
  where
    build = do
      chunks <- traverse loadChunk (adSfx defs)
      music <- traverse loadTrack (adMusic defs)
      current <- newIORef Nothing
      pure (AudioState chunks music current)
    loadChunk sd = do
      chunk <- Mixer.load (audioDir <> "/" <> sfFile sd)
      Mixer.setVolume (sfVolume sd) chunk
      pure chunk
    loadTrack md = do
      track <- Mixer.load (audioDir <> "/" <> muFile md)
      pure (track, muVolume md)

-- | Free everything (hot reload swap, shutdown). Halts playback first —
--   freeing a playing chunk is undefined behaviour in SDL_mixer.
destroyAudioState :: AudioState -> IO ()
destroyAudioState st = do
  Mixer.haltMusic
  _ <- Mixer.fadeOutMusic 0
  Mixer.halt Mixer.AllChannels
  mapM_ Mixer.free (M.elems (auChunks st))
  mapM_ (Mixer.free . fst) (M.elems (auMusic st))

-- | Close the mixer device (shutdown only).
shutdownAudio :: IO ()
shutdownAudio = Mixer.closeAudio

-- | Play the sound bound to one simulation event, if any. Overlapping
--   events share the mixer's channel pool; a full pool drops the sound
--   (never blocks the frame).
playEventSfx :: AudioState -> AudioDefs -> GameEvent -> IO ()
playEventSfx st defs ev =
  case sfxForEvent defs ev >>= \sid -> M.lookup sid (auChunks st) of
    Nothing -> pure ()
    Just chunk -> do
      result <- try (Mixer.play chunk) :: IO (Either SomeException ())
      case result of
        Left _   -> pure ()  -- all channels busy: drop, don't crash
        Right () -> pure ()

-- | Make the playing track match what the pure table wants for this mode
--   and level. Restarting only happens when the DESIRED track changes;
--   modes with no binding keep the current track (menus never restart the
--   level theme).
syncMusic :: AudioState -> AudioDefs -> GameMode -> Text -> IO ()
syncMusic st defs mode levelName = do
  current <- readIORef (auCurrent st)
  case musicFor defs mode levelName of
    Nothing -> pure ()
    Just want
      | current == Just want -> pure ()
      | otherwise -> case M.lookup want (auMusic st) of
          Nothing -> pure ()
          Just (track, vol) -> do
            Mixer.haltMusic
            Mixer.setMusicVolume vol
            Mixer.fadeInMusic 350 Mixer.Forever track
            writeIORef (auCurrent st) (Just want)
