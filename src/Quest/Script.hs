-- | Quest definitions: compiling @assets/quests/*.quest@ files.
--
--   Format:
--
--   > (quest find-key
--   >   (name "THE LOST KEY")
--   >   (auto-start)                        ; optional: active from game start
--   >   (stage collect
--   >     (goal "FIND THE GOLD KEY")        ; HUD tracker text
--   >     (objective (collect gold-key 1))
--   >     (on-complete (toast "KEY FOUND") (advance-quest find-key)))
--   >   (stage return
--   >     (goal "REACH THE EXIT")
--   >     (objective (reach-goal))
--   >     (on-complete (give-item potion-hp-s 1) (complete-quest find-key))))
--
--   Objectives: @(collect ITEM N)@, @(reach-goal)@, @(talk-to NPC)@,
--   @(kill ENEMY N)@.
--   On-complete actions use the shared vocabulary of "Script.Expr".
--   Adding a quest = adding a file. No recompilation.
module Quest.Script
  ( QuestDef(..)
  , QuestStage(..)
  , Objective(..)
  , compileQuest
  , loadQuestDir
  , questItemRefs
  , questEnemyRefs
  ) where

import qualified Data.ByteString as BS
import Data.List (sort, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import Core.Lang (LangTable, langText)
import Core.Types (ItemId(..), QuestId(..), NpcId(..), EnemyId(..))
import Script.Expr
import Script.Sexp

-- | What a stage waits for.
data Objective
  = ObjCollect !ItemId !Int
  | ObjReachGoal
  | ObjTalkTo !NpcId
  | ObjKill !EnemyId !Int
  deriving (Eq, Show)

data QuestStage = QuestStage
  { qsName       :: !Text
  , qsGoal       :: !Text        -- ^ tracker text shown on the HUD
  , qsObjective  :: !Objective
  , qsOnComplete :: ![Action]
  } deriving (Eq, Show)

data QuestDef = QuestDef
  { qdId     :: !QuestId
  , qdName   :: !Text
  , qdAuto   :: !Bool            -- ^ active from the start of a run
  , qdStages :: ![QuestStage]
  } deriving (Eq, Show)

-- | Compile one @(quest …)@ form. Display text (name/goal, say/toast in
--   on-complete) accepts string literals or 'Core.Lang' text keys.
compileQuest :: LangTable -> [Sexp] -> Either String QuestDef
compileQuest _ [] = Left "quest: (quest …) without an id"
compileQuest table (idForm : body) = do
  rawId <- maybe (Left "quest: id must be a symbol") Right (sexpSymbol idForm)
  let ctx = "quest '" <> T.unpack rawId <> "'"

  name <- case fieldOf "name" body >>= safeHead of
    Just form -> either (Left . ((ctx <> ": ") <>)) Right (langText table form)
    Nothing   -> Left (ctx <> ": missing (name …)")

  let auto = fieldOf "auto-start" body /= Nothing

  stages <- mapM (compileStage table ctx) (formsNamed "stage" body)
  if null stages
    then Left (ctx <> ": needs at least one (stage …)")
    else Right (QuestDef (QuestId rawId) name auto stages)
  where
    safeHead (x : _) = Just x
    safeHead []      = Nothing

compileStage :: LangTable -> String -> [Sexp] -> Either String QuestStage
compileStage _ ctx [] = Left (ctx <> ": (stage …) without a name")
compileStage table ctx (nameForm : body) = do
  stageName <- maybe (Left (ctx <> ": stage name must be a symbol")) Right
                 (sexpSymbol nameForm)
  let sctx = ctx <> " stage '" <> T.unpack stageName <> "'"

  goal <- case fieldOf "goal" body >>= safeHead of
    Just form -> either (Left . ((sctx <> ": ") <>)) Right (langText table form)
    Nothing   -> Left (sctx <> ": missing (goal …)")

  objective <- case fieldOf "objective" body of
    Just [form] -> compileObjective sctx form
    _           -> Left (sctx <> ": missing (objective …)")

  actions <- case fieldOf "on-complete" body of
    Nothing    -> Right []
    Just forms -> mapM (either (Left . ((sctx <> ": ") <>)) Right
                          . compileAction table)
                    forms

  Right (QuestStage stageName goal objective actions)
  where
    safeHead (x : _) = Just x
    safeHead []      = Nothing

compileObjective :: String -> Sexp -> Either String Objective
compileObjective ctx form = case form of
  SList [SSym "collect", SSym iid, SNum n] -> Right (ObjCollect (ItemId iid) (round n))
  SList [SSym "collect", SSym iid]         -> Right (ObjCollect (ItemId iid) 1)
  SList [SSym "reach-goal"]                -> Right ObjReachGoal
  SList [SSym "talk-to", SSym npc]         -> Right (ObjTalkTo (NpcId npc))
  SList [SSym "kill", SSym eid, SNum n]    -> Right (ObjKill (EnemyId eid) (round n))
  SList [SSym "kill", SSym eid]            -> Right (ObjKill (EnemyId eid) 1)
  _ -> Left (ctx <> ": unknown objective " <> show form)

-- | All item ids a quest references (objectives + actions), for startup
--   validation against the item registry.
questItemRefs :: QuestDef -> [ItemId]
questItemRefs qd = concatMap stageRefs (qdStages qd)
  where
    stageRefs st =
      objRefs (qsObjective st) <> concatMap actionItemRefs (qsOnComplete st)
    objRefs (ObjCollect iid _) = [iid]
    objRefs _                  = []

-- | All enemy ids a quest references, for startup validation against the
--   enemy definitions.
questEnemyRefs :: QuestDef -> [EnemyId]
questEnemyRefs qd =
  [ eid | st <- qdStages qd, ObjKill eid _ <- [qsObjective st] ]

-- | Load and compile every @*.quest@ file in a directory (sorted by name).
--   A missing directory simply means no quests.
loadQuestDir :: LangTable -> FilePath -> IO (Either String [QuestDef])
loadQuestDir table dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure (Right [])
    else do
      entries <- listDirectory dir
      let files = [ dir </> e | e <- sort entries, ".quest" `isSuffixOf` e ]
      results <- mapM loadOne files
      pure (concat <$> sequence results)
  where
    loadOne path = do
      bytes <- BS.readFile path
      pure $ case decodeUtf8' bytes of
        Left err -> Left (path <> ": not valid UTF-8 (" <> show err <> ")")
        Right txt -> do
          forms <- either (Left . ((path <> ": ") <>)) Right (parseSexps txt)
          mapM (either (Left . ((path <> ": ") <>)) Right . compileQuest table)
               (formsNamed "quest" forms)
