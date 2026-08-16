-- | Save-game serialization: a single 'SaveGame' record to/from JSON.
--   'decodeSave . encodeSave ≡ Right' is the core roundtrip invariant
--   (tested). Corrupt files are rejected whole — the game never half-loads
--   a save.
--
--   Versioning: 'currentSaveVersion' is bumped on every schema change and a
--   step is appended to 'migrations'; old saves are then upgraded value-by-
--   value on load instead of rejected. Only FUTURE versions are refused.
--
--   Content references (quest ids, item ids) can still go stale when assets
--   change between sessions — 'sanitizeSave' drops or clamps them explicitly
--   with human-readable warnings, never silently pointing at the wrong
--   thing.
module Save.Codec
  ( SaveGame(..)
  , SaveEnv(..)
  , currentSaveVersion
  , encodeSave
  , decodeSave
  , sanitizeSave
  , slotSummary
  , slotPath
  , writeSlot
  , readSlot
  ) where

import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as A
import qualified Data.ByteString.Lazy as BL
import Control.Exception (try, IOException)
import Control.Monad (foldM)
import Data.List (elemIndex)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.Printf (printf)

import Core.Types
import Sim.Spawn (PlayerPersist(..))

-- | Bump when the schema changes AND append a step to 'migrations'.
--
--   History: v1 stored the level as a sort-order INDEX (inserting a level
--   file silently retargeted every old save); v2 stores the level file's
--   base name. v3 added the talent system: kill stats, talent points/ranks
--   and the max-HP/stamina bonuses inside the derived block.
currentSaveVersion :: Int
currentSaveVersion = 3

-- | What the codec needs from the running game: the discovered level base
--   names, in play order.
newtype SaveEnv = SaveEnv { seLevelNames :: [String] }
  deriving (Eq, Show)

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

encodeSave :: SaveEnv -> SaveGame -> BL.ByteString
encodeSave env sv = A.encode $ A.object
  [ "version"   .= svVersion sv
  , "levelName" .= levelNameOf env (svLevel sv)
  , "stats"     .= statsV (svStats sv)
  , "player"    .= playerV (svPlayer sv)
  , "quests"    .= questsV (svQuests sv)
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
      [ "deaths" .= statDeaths st, "items" .= statItems st
      , "kills" .= statKills st, "time" .= statTime st ]
    playerV pp = A.object
      [ "backpack" .= M.fromList
          [ (raw, n) | (ItemId raw, n) <- M.toList (ppBackpack pp) ]
      , "equipped" .= M.fromList
          [ (slotKey slot, raw) | (slot, ItemId raw) <- M.toList (ppEquipped pp) ]
      , "vitals"   .= vitalsV (ppVitals pp)
      , "derived"  .= derivedV (ppStats pp)
      , "talents"  .= talentsV (ppTalents pp)
      ]
    vitalsV v = A.object
      [ "hp" .= vHp v, "maxHp" .= vMaxHp v
      , "mp" .= vMp v, "maxMp" .= vMaxMp v
      , "stamina" .= vStamina v, "maxStamina" .= vMaxStamina v
      ]
    derivedV d = A.object
      [ "atk" .= dsAtk d, "def" .= dsDef d, "speedMult" .= dsSpeedMult d
      , "maxHp" .= dsMaxHp d, "maxStamina" .= dsMaxStamina d ]
    talentsV ts = A.object
      [ "points" .= tsPoints ts
      , "ranks"  .= M.fromList
          [ (raw, n) | (TalentId raw, n) <- M.toList (tsRanks ts) ]
      ]

-- | The clamped level base name for an index (encode side).
levelNameOf :: SaveEnv -> Int -> String
levelNameOf (SaveEnv names) ix = case drop ix names of
  (n : _) -> n
  []      -> case reverse names of { (n : _) -> n; [] -> "" }

--------------------------------------------------------------------------------
-- Migrations
--------------------------------------------------------------------------------

-- | One entry per historical version: upgrade a version-N object to N+1.
--   Loading folds the chain from the file's version up to
--   'currentSaveVersion', so any past save keeps working.
migrations :: SaveEnv -> [(Int, A.Object -> A.Parser A.Object)]
migrations env =
  [ (1, migrate1to2 env)
  , (2, migrate2to3)
  ]

-- | v1 -> v2: the level was stored as a sort-order index; replace it with
--   the level's base name (out-of-range indices are a hard error — better a
--   loud CORRUPT slot than loading the wrong level).
migrate1to2 :: SaveEnv -> A.Object -> A.Parser A.Object
migrate1to2 (SaveEnv names) o = do
  ix <- o .: "level"
  case drop ix names of
    (name : _) | ix >= 0 ->
      pure . KM.insert "levelName" (A.toJSON name) . KM.delete "level" $ o
    _ -> fail ("migration v1->v2: level index " <> show (ix :: Int)
               <> " is out of range (have " <> show (length names) <> " levels)")

-- | v2 -> v3 (talent system): zero kills into the stats, an empty talent
--   block onto the player, and zero max-HP/stamina bonuses into the derived
--   block. An old save simply starts with no talents.
migrate2to3 :: A.Object -> A.Parser A.Object
migrate2to3 o = do
  stats   <- o .: "stats"   :: A.Parser A.Object
  player  <- o .: "player"  :: A.Parser A.Object
  derived <- player .: "derived" :: A.Parser A.Object
  let stats'   = KM.insert "kills" (A.toJSON (0 :: Int)) stats
      derived' = KM.insert "maxHp" (A.toJSON (0 :: Int))
               . KM.insert "maxStamina" (A.toJSON (0 :: Int)) $ derived
      talents  = A.object [ "points" .= (0 :: Int)
                          , "ranks" .= (M.empty :: M.Map Text Int) ]
      player'  = KM.insert "derived" (A.Object derived')
               . KM.insert "talents" talents $ player
  pure . KM.insert "stats" (A.Object stats')
       . KM.insert "player" (A.Object player') $ o

-- | Run every applicable migration step, then bump the version field.
migrate :: SaveEnv -> Int -> A.Object -> A.Parser A.Object
migrate env from o = do
  o' <- foldM step o [from .. currentSaveVersion - 1]
  pure (KM.insert "version" (A.toJSON currentSaveVersion) o')
  where
    step obj v = case lookup v (migrations env) of
      Just f  -> f obj
      Nothing -> fail ("no migration from save version " <> show v)

decodeSave :: SaveEnv -> BL.ByteString -> Either String SaveGame
decodeSave env bytes = do
  value <- A.eitherDecode bytes
  A.parseEither parseSave value
  where
    parseSave = A.withObject "SaveGame" $ \o0 -> do
      version <- o0 .: "version"
      if version > currentSaveVersion
        then fail ("save version " <> show (version :: Int)
                   <> " is newer than this game (expected <= "
                   <> show currentSaveVersion <> ")")
        else do
          o <- migrate env version o0
          levelName <- o .: "levelName"
          level <- case elemIndex (levelName :: String) (seLevelNames env) of
            Just ix -> pure ix
            Nothing -> fail ("save references unknown level '" <> levelName <> "'")
          stats <- o .: "stats" >>= parseStats
          player <- o .: "player" >>= parsePlayer
          quests <- o .: "quests" >>= parseQuests
          pure (SaveGame currentSaveVersion level stats player quests)

    parseStats = A.withObject "stats" $ \o ->
      RunStats <$> o .: "deaths" <*> o .: "items" <*> o .: "kills"
               <*> o .: "time"

    parsePlayer = A.withObject "player" $ \o -> do
      bpRaw <- o .: "backpack" :: A.Parser (M.Map Text Int)
      eqRaw <- o .: "equipped" :: A.Parser (M.Map Text Text)
      vitals <- o .: "vitals" >>= parseVitals
      derived <- o .: "derived" >>= parseDerived
      talents <- o .: "talents" >>= parseTalents
      equipped <- M.fromList <$> mapM
        (\(k, v) -> (\slot -> (slot, ItemId v)) <$> parseSlotKey k)
        (M.toList eqRaw)
      pure PlayerPersist
        { ppBackpack = M.fromList [ (ItemId k, n) | (k, n) <- M.toList bpRaw ]
        , ppEquipped = equipped
        , ppVitals = vitals
        , ppStats = derived
        , ppTalents = talents
        }

    parseVitals = A.withObject "vitals" $ \o ->
      Vitals <$> o .: "hp" <*> o .: "maxHp" <*> o .: "mp" <*> o .: "maxMp"
             <*> o .: "stamina" <*> o .: "maxStamina"

    parseDerived = A.withObject "derived" $ \o ->
      DerivedStats <$> o .: "atk" <*> o .: "def" <*> o .: "speedMult"
                   <*> o .: "maxHp" <*> o .: "maxStamina"

    parseTalents = A.withObject "talents" $ \o -> do
      pts <- o .: "points"
      ranksRaw <- o .: "ranks" :: A.Parser (M.Map Text Int)
      pure TalentState
        { tsPoints = pts
        , tsRanks = M.fromList [ (TalentId k, n) | (k, n) <- M.toList ranksRaw ]
        }

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
-- Sanitization (stale content references)
--------------------------------------------------------------------------------

-- | Drop references to content that no longer exists and clamp what can be
--   clamped, returning warnings for everything touched. Content ids can go
--   stale between sessions (a quest renamed, an item removed); loading must
--   handle that EXPLICITLY instead of silently tracking the wrong thing.
sanitizeSave
  :: [(QuestId, Int)]        -- ^ known quests with their stage counts
  -> [ItemId]                -- ^ known item ids
  -> [(TalentId, Int, Int)]  -- ^ known talents: (id, max rank, cost per rank)
  -> SaveGame
  -> (SaveGame, [String])
sanitizeSave knownQuests knownItems knownTalents sv =
  ( sv { svQuests = QuestLog (M.fromList keptQuests) (qlFlags qlog)
       , svPlayer = player { ppBackpack = keptBackpack, ppEquipped = keptEquipped
                           , ppTalents = keptTalents }
       }
  , questWarns <> bagWarns <> eqWarns <> talentWarns
  )
  where
    qlog = svQuests sv
    player = svPlayer sv

    -- Ranks in talents that no longer exist refund one point per rank (the
    -- original cost is unknowable); ranks above the current max refund at
    -- the talent's cost. Every adjustment is reported.
    (keptTalents, talentWarns) =
      let ts0 = ppTalents player
          step (tid@(TalentId raw), rank) (ranks, refund, warns) =
            case [ (mr, c) | (kid, mr, c) <- knownTalents, kid == tid ] of
              [] ->
                ( ranks, refund + rank
                , ("save: refunded " <> show rank
                   <> " point(s) from unknown talent '" <> T.unpack raw <> "'") : warns )
              ((maxRank, cost) : _)
                | rank > maxRank ->
                    ( M.insert tid maxRank ranks
                    , refund + (rank - maxRank) * cost
                    , ("save: clamped rank of talent '" <> T.unpack raw <> "'") : warns )
                | otherwise -> (M.insert tid rank ranks, refund, warns)
          (ranks', refund', warns') =
            foldr step (M.empty, 0, []) (M.toList (tsRanks ts0))
      in (TalentState (tsPoints ts0 + refund') ranks', warns')

    (keptQuests, questWarns) = foldr checkQuest ([], []) (M.toList (qlQuests qlog))
    checkQuest (qid@(QuestId raw), prog) (keep, warns) =
      case lookup qid knownQuests of
        Nothing -> (keep, ("save: dropped unknown quest '" <> T.unpack raw <> "'") : warns)
        Just stageCount
          | qpStage prog >= stageCount ->
              ( (qid, prog { qpStage = max 0 (stageCount - 1) }) : keep
              , ("save: clamped stage of quest '" <> T.unpack raw <> "'") : warns )
          | otherwise -> ((qid, prog) : keep, warns)

    (keptBackpack, bagWarns) =
      let (bad, good) = M.partitionWithKey (\iid _ -> iid `notElem` knownItems)
                          (ppBackpack player)
      in (good, [ "save: dropped unknown item '" <> T.unpack raw <> "'"
                | ItemId raw <- M.keys bad ])

    (keptEquipped, eqWarns) =
      let (bad, good) = M.partition (`notElem` knownItems) (ppEquipped player)
      in (good, [ "save: unequipped unknown item '" <> T.unpack raw <> "'"
                | ItemId raw <- M.elems bad ])

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

writeSlot :: SaveEnv -> Int -> SaveGame -> IO ()
writeSlot env n sv = do
  createDirectoryIfMissing True "saves"
  BL.writeFile (slotPath n) (encodeSave env sv)

-- | Nothing = slot empty; Left = present but unreadable (corrupt). Old
--   versions are migrated transparently.
readSlot :: SaveEnv -> Int -> IO (Maybe (Either String SaveGame))
readSlot env n = do
  let path = slotPath n
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try (BL.readFile path) :: IO (Either IOException BL.ByteString)
      pure . Just $ case result of
        Left err    -> Left (show err)
        Right bytes -> decodeSave env bytes
