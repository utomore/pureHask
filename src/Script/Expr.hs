-- | The shared condition/action vocabulary of the script DSL. Quest scripts
--   and NPC scripts both compile their logic through this module (see
--   docs/SYSTEMS_DESIGN.md §1.3 — that table IS this module's spec; register
--   new vocabulary there before extending here).
--
--   Design rules:
--
--     * The DSL stays declarative: no loops, no variables, no arithmetic.
--       Complex logic becomes a new primitive here (typed, tested), never a
--       Turing tarpit in the script files.
--     * 'evalCond' is a pure read of a 'ScriptEnv' snapshot.
--     * Actions EXECUTE nothing; interpreters translate them into effects
--       ('Quest.Runtime' internal transitions or 'WorldCommand's).
module Script.Expr
  ( Cond(..)
  , Action(..)
  , ScriptEnv(..)
  , emptyScriptEnv
  , evalCond
  , compileCond
  , compileAction
    -- * Reference collection (for startup cross-validation)
  , condItemRefs
  , actionItemRefs
  , condQuestRefs
  , actionQuestRefs
  , actionNpcRefs
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Linear (V2(..), norm)

import Core.Config (tileSize)
import Core.Lang (LangTable, langText)
import Core.Types (ItemId(..), QuestId(..), NpcId(..), QuestPhase(..))
import Script.Sexp

-- | Conditions scripts can test.
data Cond
  = CFlag !Text
  | CNot !Cond
  | CAnd ![Cond]
  | COr ![Cond]
  | CHasItem !ItemId !Int
  | CQuestState !QuestId !QuestPhase
  | CPlayerNear !Double !Double !Double  -- ^ tile x, tile y, radius in tiles
  | CLevelIs !Text
  deriving (Eq, Show)

-- | Actions scripts can request.
data Action
  = ASetFlag !Text
  | AClearFlag !Text
  | AGiveItem !ItemId !Int
  | ATakeItem !ItemId !Int
  | ASay !Text ![Text]           -- ^ speaker name, dialogue lines
  | AToast !Text
  | AOfferQuest !QuestId
  | AAdvanceQuest !QuestId
  | ACompleteQuest !QuestId
  | ASpawnNpc !NpcId !Double !Double  -- ^ tile coordinates
  | ADespawnNpc !NpcId
  deriving (Eq, Show)

-- | The read-only world snapshot conditions are evaluated against.
data ScriptEnv = ScriptEnv
  { envFlags      :: !(Set Text)
  , envBackpack   :: !(Map ItemId Int)
  , envQuestPhase :: !(Map QuestId QuestPhase)  -- ^ absent = 'QAvailable'
  , envPlayerPos  :: !(V2 Double)               -- ^ player center, pixels
  , envLevelName  :: !Text
  } deriving (Eq, Show)

emptyScriptEnv :: ScriptEnv
emptyScriptEnv = ScriptEnv S.empty M.empty M.empty (V2 0 0) ""

-- | Evaluate a condition against the snapshot. Total and pure.
evalCond :: ScriptEnv -> Cond -> Bool
evalCond env cond = case cond of
  CFlag name        -> S.member name (envFlags env)
  CNot c            -> not (evalCond env c)
  CAnd cs           -> all (evalCond env) cs
  COr cs            -> any (evalCond env) cs
  CHasItem iid n    -> M.findWithDefault 0 iid (envBackpack env) >= n
  CQuestState q ph  -> M.findWithDefault QAvailable q (envQuestPhase env) == ph
  CPlayerNear tx ty r ->
    let target = V2 (tx * tileSize) (ty * tileSize)
    in norm (envPlayerPos env - target) <= r * tileSize
  CLevelIs name     -> envLevelName env == name

--------------------------------------------------------------------------------
-- Compilation from S-expressions
--------------------------------------------------------------------------------

-- | Compile one condition form, e.g. @(and (flag met-elder) (has-item gold-key 1))@.
compileCond :: Sexp -> Either String Cond
compileCond form = case form of
  SList (SSym "and" : rest) -> CAnd <$> mapM compileCond rest
  SList (SSym "or" : rest)  -> COr <$> mapM compileCond rest
  SList [SSym "not", c]     -> CNot <$> compileCond c
  SList [SSym "flag", SSym name] -> Right (CFlag name)
  SList [SSym "has-item", SSym iid, SNum n] ->
    Right (CHasItem (ItemId iid) (round n))
  SList [SSym "has-item", SSym iid] ->
    Right (CHasItem (ItemId iid) 1)
  SList [SSym "quest-state", SSym q, SSym phase] -> case phase of
    "available" -> Right (CQuestState (QuestId q) QAvailable)
    "active"    -> Right (CQuestState (QuestId q) QActive)
    "done"      -> Right (CQuestState (QuestId q) QDone)
    _           -> Left ("unknown quest phase in " <> show form)
  SList [SSym "player-near", SNum x, SNum y, SNum r] ->
    Right (CPlayerNear x y r)
  SList [SSym "level-is", nameForm]
    | Just name <- sexpString nameForm -> Right (CLevelIs name)
  _ -> Left ("unknown condition " <> show form)

-- | Compile one action form, e.g. @(give-item potion-hp-s 2)@. Display-text
--   positions (@say@ lines, @toast@ messages) accept string literals or
--   text keys resolved against the 'LangTable'.
compileAction :: LangTable -> Sexp -> Either String Action
compileAction table form = case form of
  SList [SSym "set-flag", SSym name]   -> Right (ASetFlag name)
  SList [SSym "clear-flag", SSym name] -> Right (AClearFlag name)
  SList [SSym "give-item", SSym iid, SNum n] -> Right (AGiveItem (ItemId iid) (round n))
  SList [SSym "give-item", SSym iid]         -> Right (AGiveItem (ItemId iid) 1)
  SList [SSym "take-item", SSym iid, SNum n] -> Right (ATakeItem (ItemId iid) (round n))
  SList [SSym "take-item", SSym iid]         -> Right (ATakeItem (ItemId iid) 1)
  SList (SSym "say" : SSym speaker : rest)
    | not (null rest) -> ASay speaker <$> mapM (langText table) rest
  SList [SSym "toast", msgForm] -> AToast <$> langText table msgForm
  SList [SSym "offer-quest", SSym q]    -> Right (AOfferQuest (QuestId q))
  SList [SSym "advance-quest", SSym q]  -> Right (AAdvanceQuest (QuestId q))
  SList [SSym "complete-quest", SSym q] -> Right (ACompleteQuest (QuestId q))
  SList [SSym "spawn-npc", SSym n, SNum x, SNum y] ->
    Right (ASpawnNpc (NpcId n) x y)
  SList [SSym "despawn-npc", SSym n] -> Right (ADespawnNpc (NpcId n))
  _ -> Left ("unknown action " <> show form)

--------------------------------------------------------------------------------
-- Reference collection (startup cross-validation)
--------------------------------------------------------------------------------

condItemRefs :: Cond -> [ItemId]
condItemRefs cond = case cond of
  CHasItem iid _ -> [iid]
  CNot c         -> condItemRefs c
  CAnd cs        -> concatMap condItemRefs cs
  COr cs         -> concatMap condItemRefs cs
  _              -> []

condQuestRefs :: Cond -> [QuestId]
condQuestRefs cond = case cond of
  CQuestState q _ -> [q]
  CNot c          -> condQuestRefs c
  CAnd cs         -> concatMap condQuestRefs cs
  COr cs          -> concatMap condQuestRefs cs
  _               -> []

actionItemRefs :: Action -> [ItemId]
actionItemRefs act = case act of
  AGiveItem iid _ -> [iid]
  ATakeItem iid _ -> [iid]
  _               -> []

actionQuestRefs :: Action -> [QuestId]
actionQuestRefs act = case act of
  AOfferQuest q    -> [q]
  AAdvanceQuest q  -> [q]
  ACompleteQuest q -> [q]
  _                -> []

actionNpcRefs :: Action -> [NpcId]
actionNpcRefs act = case act of
  ASpawnNpc n _ _ -> [n]
  ADespawnNpc n   -> [n]
  _               -> []
