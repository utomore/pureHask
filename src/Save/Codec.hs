-- | Save-game serialization: a single 'SaveGame' record to/from JSON.
--   'decodeSave . encodeSave ≡ Right' is the core roundtrip invariant
--   (tested). A version mismatch or corrupt file is rejected whole — the
--   game never half-loads a save.
module Save.Codec
  ( SaveGame(..)
  , currentSaveVersion
  , encodeSave
  , decodeSave
  , slotSummary
  , slotPath
  , writeSlot
  , readSlot
  ) where

import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import Control.Exception (try, IOException)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)

import Core.Types
import Sim.Spawn (PlayerPersist(..))

-- | Bump when the schema changes; old saves are then rejected loudly.
currentSaveVersion :: Int
currentSaveVersion = 1

-- | Everything a save file holds.
data SaveGame = SaveGame
  { svVersion :: !Int
  , svLevel   :: !Int
  , svStats   :: !RunStats
  , svPlayer  :: !PlayerPersist
  , svQuests  :: !QuestLog
  } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Encoding (hand-written; no orphan instances on Core.Types)
--------------------------------------------------------------------------------

encodeSave :: SaveGame -> BL.ByteString
encodeSave sv = A.encode $ A.object
  [ "version"  .= svVersion sv
  , "level"    .= svLevel sv
  , "stats"    .= statsV (svStats sv)
  , "player"   .= playerV (svPlayer sv)
  , "quests"   .= questsV (svQuests sv)
  ]
  where
    questsV qlog = A.object
      [ "flags" .= (qlFlags qlog)
      , "progress" .= M.fromList
          [ (raw, A.object [ "phase" .= phaseKey (qpPhase p)
                           , "stage" .= qpStage p
                           , "count" .= qpCount p ])
          | (QuestId raw, p) <- M.toList (qlQuests qlog)
          ]
      ]
    phaseKey :: QuestPhase -> Text
    phaseKey QAvailable = "available"
    phaseKey QActive    = "active"
    phaseKey QDone      = "done"
    statsV st = A.object
      [ "deaths" .= statDeaths st, "items" .= statItems st, "time" .= statTime st ]
    playerV pp = A.object
      [ "backpack" .= M.fromList
          [ (raw, n) | (ItemId raw, n) <- M.toList (ppBackpack pp) ]
      , "equipped" .= M.fromList
          [ (slotKey slot, raw) | (slot, ItemId raw) <- M.toList (ppEquipped pp) ]
      , "vitals"   .= vitalsV (ppVitals pp)
      , "derived"  .= derivedV (ppStats pp)
      ]
    vitalsV v = A.object
      [ "hp" .= vHp v, "maxHp" .= vMaxHp v
      , "mp" .= vMp v, "maxMp" .= vMaxMp v
      , "stamina" .= vStamina v, "maxStamina" .= vMaxStamina v
      ]
    derivedV d = A.object
      [ "atk" .= dsAtk d, "def" .= dsDef d, "speedMult" .= dsSpeedMult d ]

decodeSave :: BL.ByteString -> Either String SaveGame
decodeSave bytes = do
  value <- A.eitherDecode bytes
  A.parseEither parseSave value
  where
    parseSave = A.withObject "SaveGame" $ \o -> do
      version <- o .: "version"
      if version /= currentSaveVersion
        then fail ("unsupported save version " <> show (version :: Int)
                   <> " (expected " <> show currentSaveVersion <> ")")
        else do
          level <- o .: "level"
          stats <- o .: "stats" >>= parseStats
          player <- o .: "player" >>= parsePlayer
          quests <- o .: "quests" >>= parseQuests
          pure (SaveGame version level stats player quests)

    parseStats = A.withObject "stats" $ \o ->
      RunStats <$> o .: "deaths" <*> o .: "items" <*> o .: "time"

    parsePlayer = A.withObject "player" $ \o -> do
      bpRaw <- o .: "backpack" :: A.Parser (M.Map Text Int)
      eqRaw <- o .: "equipped" :: A.Parser (M.Map Text Text)
      vitals <- o .: "vitals" >>= parseVitals
      derived <- o .: "derived" >>= parseDerived
      equipped <- M.fromList <$> mapM
        (\(k, v) -> (\slot -> (slot, ItemId v)) <$> parseSlotKey k)
        (M.toList eqRaw)
      pure PlayerPersist
        { ppBackpack = M.fromList [ (ItemId k, n) | (k, n) <- M.toList bpRaw ]
        , ppEquipped = equipped
        , ppVitals = vitals
        , ppStats = derived
        }

    parseVitals = A.withObject "vitals" $ \o ->
      Vitals <$> o .: "hp" <*> o .: "maxHp" <*> o .: "mp" <*> o .: "maxMp"
             <*> o .: "stamina" <*> o .: "maxStamina"

    parseDerived = A.withObject "derived" $ \o ->
      DerivedStats <$> o .: "atk" <*> o .: "def" <*> o .: "speedMult"

    parseQuests = A.withObject "quests" $ \o -> do
      flags <- o .: "flags"
      progRaw <- o .: "progress" :: A.Parser (M.Map Text A.Value)
      progress <- mapM parseProgress (M.toList progRaw)
      pure (QuestLog (M.fromList progress) flags)

    parseProgress (raw, val) =
      flip (A.withObject "questProgress") val $ \o -> do
        phaseRaw <- o .: "phase"
        phase <- case (phaseRaw :: Text) of
          "available" -> pure QAvailable
          "active"    -> pure QActive
          "done"      -> pure QDone
          other       -> fail ("unknown quest phase " <> show other)
        stage <- o .: "stage"
        count <- o .: "count"
        pure (QuestId raw, QuestProgress phase stage count)

slotKey :: EquipSlot -> Text
slotKey slot = case slot of
  SlotWeapon -> "weapon"
  SlotBody   -> "body"
  SlotShoes  -> "shoes"
  SlotGloves -> "gloves"
  SlotHead   -> "head"

parseSlotKey :: Text -> A.Parser EquipSlot
parseSlotKey t = case t of
  "weapon" -> pure SlotWeapon
  "body"   -> pure SlotBody
  "shoes"  -> pure SlotShoes
  "gloves" -> pure SlotGloves
  "head"   -> pure SlotHead
  other    -> fail ("unknown equip slot key " <> T.unpack other)

--------------------------------------------------------------------------------
-- Files
--------------------------------------------------------------------------------

-- | Slot 0 is the autosave; 1..3 are the manual slots.
slotPath :: Int -> FilePath
slotPath 0 = "saves" </> "auto.json"
slotPath n = "saves" </> ("slot" <> show n <> ".json")

-- | One-line description shown in the save/load menu.
slotSummary :: SaveGame -> String
slotSummary sv =
  let total = floor (statTime (svStats sv)) :: Int
      (m, s) = total `divMod` 60
  in printf "LV %d  %02d:%02d" (svLevel sv + 1) m s

writeSlot :: Int -> SaveGame -> IO ()
writeSlot n sv = do
  createDirectoryIfMissing True "saves"
  BL.writeFile (slotPath n) (encodeSave sv)

-- | Nothing = slot empty; Left = present but unreadable (corrupt/old).
readSlot :: Int -> IO (Maybe (Either String SaveGame))
readSlot n = do
  let path = slotPath n
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
      pure . Just $ case result of
        Left err    -> Left (show err)
        Right bytes -> decodeSave bytes
