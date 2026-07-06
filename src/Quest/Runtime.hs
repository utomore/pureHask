-- | The quest machine: folds simulation 'GameEvent's into the 'QuestLog' and
--   translates script actions into 'WorldCommand's and HUD toasts. Pure;
--   folded by "FRP.Network" (via "Game.Logic"), unit-tested directly.
module Quest.Runtime
  ( QuestOut(..)
  , QuestText(..)
  , defaultQuestText
  , initialQuestLog
  , stepQuests
  , applyQuestAction
  , activeGoal
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)

import Core.Types
import Quest.Script
import Script.Expr

-- | Result of feeding events to the quest machine.
data QuestOut = QuestOut
  { qoLog      :: !QuestLog
  , qoCommands :: ![WorldCommand]
  , qoToasts   :: ![Text]
  } deriving (Eq, Show)

-- | The engine-generated toast prefixes, resolved from the language table
--   at startup (keys @ui.quest.started@ / @ui.quest.completed@). The quest
--   machine stays pure — text is data fed in, never looked up here.
data QuestText = QuestText
  { qtStarted   :: !Text
  , qtCompleted :: !Text
  } deriving (Eq, Show)

-- | Fallback wording, also what the unit tests run against.
defaultQuestText :: QuestText
defaultQuestText = QuestText "任務開始:" "任務完成:"

-- | Fresh log: auto-start quests begin active.
initialQuestLog :: [QuestDef] -> QuestLog
initialQuestLog defs = QuestLog
  { qlQuests = M.fromList
      [ (qdId qd, QuestProgress QActive 0 0) | qd <- defs, qdAuto qd ]
  , qlFlags = S.empty
  }

progressOf :: QuestLog -> QuestId -> QuestProgress
progressOf qlog qid =
  M.findWithDefault (QuestProgress QAvailable 0 0) qid (qlQuests qlog)

-- | Feed one frame's simulation events through every quest.
stepQuests :: QuestText -> [QuestDef] -> [GameEvent] -> QuestLog -> QuestOut
stepQuests qtext defs evs qlog0 = foldl handleEvent (QuestOut qlog0 [] []) evs
  where
    handleEvent out ev = case ev of
      -- A loaded save replaces the whole log.
      EvRunRestored _ _ savedLog -> out { qoLog = savedLog }
      _ -> foldl (progressQuest ev) out defs

    progressQuest ev out qd =
      let qlog = qoLog out
          prog = progressOf qlog (qdId qd)
      in case (qpPhase prog, drop (qpStage prog) (qdStages qd)) of
           (QActive, stage : _) ->
             case objectiveTick ev (qsObjective stage) (qpCount prog) of
               NoProgress   -> out
               Counted n    ->
                 out { qoLog = putProgress qlog (qdId qd) prog { qpCount = n } }
               Fulfilled    ->
                 let cleared = putProgress qlog (qdId qd) prog { qpCount = 0 }
                     out' = out { qoLog = cleared }
                 in foldl (runAction qd) out' (qsOnComplete stage)
           _ -> out

    runAction _qd out act = applyQuestAction qtext defs act out

data ObjectiveTick = NoProgress | Counted !Int | Fulfilled

-- | How one event advances one objective.
objectiveTick :: GameEvent -> Objective -> Int -> ObjectiveTick
objectiveTick ev obj count = case (ev, obj) of
  (EvItemPicked iid, ObjCollect want n)
    | iid == want ->
        if count + 1 >= n then Fulfilled else Counted (count + 1)
  (EvGoalReached, ObjReachGoal) -> Fulfilled
  (EvTalkedTo npc, ObjTalkTo want)
    | npc == want -> Fulfilled
  (EvEnemyKilled eid, ObjKill want n)
    | eid == want ->
        if count + 1 >= n then Fulfilled else Counted (count + 1)
  _ -> NoProgress

-- | Interpret one script action in the quest layer. Quest/flag actions edit
--   the log; world actions become commands; dialogue and toasts surface to
--   the HUD.
applyQuestAction :: QuestText -> [QuestDef] -> Action -> QuestOut -> QuestOut
applyQuestAction qtext defs act out = case act of
  ASetFlag name   -> mapLog (\l -> l { qlFlags = S.insert name (qlFlags l) }) out
  AClearFlag name -> mapLog (\l -> l { qlFlags = S.delete name (qlFlags l) }) out
  AGiveItem iid n -> out { qoCommands = qoCommands out <> [WcGiveItem iid n] }
  ATakeItem iid n -> out { qoCommands = qoCommands out <> [WcTakeItem iid n] }
  ASpawnNpc npc x y -> out { qoCommands = qoCommands out <> [WcSpawnNpc npc x y] }
  ADespawnNpc npc -> out { qoCommands = qoCommands out <> [WcDespawnNpc npc] }
  AToast msg      -> out { qoToasts = qoToasts out <> [msg] }
  -- Dialogue from quest scripts is shown as toast lines; NPC dialogue proper
  -- arrives with the NPC system.
  ASay speaker lns ->
    out { qoToasts = qoToasts out <> [speaker <> ": " <> l | l <- lns] }
  AOfferQuest q ->
    let prog = progressOf (qoLog out) q
    in case qpPhase prog of
         QAvailable ->
           withToast (qtStarted qtext <> nameOf q)
             (mapLog (\l -> putProgress l q (QuestProgress QActive 0 0)) out)
         _ -> out
  AAdvanceQuest q ->
    let prog = progressOf (qoLog out) q
        next = qpStage prog + 1
        stageCount = maybe 0 (length . qdStages) (defOf q)
    in if next >= stageCount
         then complete q out
         else mapLog
                (\l -> putProgress l q prog { qpStage = next, qpCount = 0 }) out
  ACompleteQuest q -> complete q out
  where
    mapLog f o = o { qoLog = f (qoLog o) }
    withToast t o = o { qoToasts = qoToasts o <> [t] }
    defOf q = case [ d | d <- defs, qdId d == q ] of
      (d : _) -> Just d
      []      -> Nothing
    nameOf q = maybe (let QuestId raw = q in raw) qdName (defOf q)
    complete q o =
      withToast (qtCompleted qtext <> nameOf q)
        (mapLog (\l -> putProgress l q
                  (progressOf (qoLog o) q) { qpPhase = QDone }) o)

putProgress :: QuestLog -> QuestId -> QuestProgress -> QuestLog
putProgress qlog qid prog = qlog { qlQuests = M.insert qid prog (qlQuests qlog) }

-- | The HUD tracker line: the first active quest's current goal.
activeGoal :: [QuestDef] -> QuestLog -> Maybe (Text, Text)
activeGoal defs qlog =
  case [ (qdName qd, qsGoal stage)
       | qd <- defs
       , let prog = progressOf qlog (qdId qd)
       , qpPhase prog == QActive
       , stage <- take 1 (drop (qpStage prog) (qdStages qd))
       ] of
    (x : _) -> Just x
    []      -> Nothing
