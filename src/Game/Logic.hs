-- | Composition of the pure logic machines — flow ("Flow.Machine"), quests
--   ("Quest.Runtime"), NPC dialogue ("Npc.Core") and HUD toasts
--   ("Hud.Machine") — into one step function that "FRP.Network" folds. This
--   module only sequences the machines and merges their outputs; each
--   machine's rules live (and are tested) in its own module.
module Game.Logic
  ( LogicState(..)
  , LogicOut(..)
  , initialLogic
  , stepLogic
  , scriptEnvOf
  ) where

import qualified Data.Map.Strict as M
import Data.Text (Text)

import Core.Types
import Flow.Machine
import Hud.Machine
import Npc.Core (evalDialogue)
import Npc.Script (NpcDef, ndName)
import Quest.Runtime
import Quest.Script (QuestDef)
import Script.Expr

data LogicState = LogicState
  { lsFlow   :: !FlowState
  , lsQuests :: !QuestLog
  , lsHud    :: !HudState
  , lsWorld  :: !WorldSnapshot  -- ^ last frame's world facts, for script conds
  } deriving (Eq, Show)

data LogicOut = LogicOut
  { loFlowCmds  :: ![FlowCommand]
  , loWorldCmds :: ![WorldCommand]
  } deriving (Eq, Show)

initialLogic :: [QuestDef] -> Int -> LogicState
initialLogic defs levelCount = LogicState
  { lsFlow   = initialFlow levelCount
  , lsQuests = initialQuestLog defs
  , lsHud    = emptyHud
  , lsWorld  = emptyWorldSnapshot
  }

-- | The read-only snapshot dialogue conditions are evaluated against.
scriptEnvOf :: LogicState -> ScriptEnv
scriptEnvOf ls = ScriptEnv
  { envFlags      = qlFlags (lsQuests ls)
  , envBackpack   = wsBackpack (lsWorld ls)
  , envQuestPhase = M.map qpPhase (qlQuests (lsQuests ls))
  , envPlayerPos  = wsPlayerPos (lsWorld ls)
  , envLevelName  = wsLevelName (lsWorld ls)
  }

-- | One stimulus through all machines. 'QuestText' carries the language
--   table's engine strings (toast prefixes) so the machines stay pure.
stepLogic :: QuestText -> [QuestDef] -> M.Map NpcId NpcDef -> FlowIn -> LogicState
          -> (LogicState, LogicOut)
stepLogic qtext questDefs npcDefs flowIn ls = case flowIn of
  FlowFrame dt _ _ world ->
    let (flow', cmds) = stepFlow flowIn (lsFlow ls)
        hud' = tickHud dt (lsHud ls)
        -- A fresh run resets quest progress along with the stats.
        quests' = if CmdNewGame `elem` cmds
                    then initialQuestLog questDefs
                    else lsQuests ls
    in ( ls { lsFlow = flow', lsHud = hud', lsQuests = quests', lsWorld = world }
       , LogicOut cmds [] )

  FlowEvents evs ->
    let (flow', cmds) = stepFlow flowIn (lsFlow ls)
        QuestOut qlog wcmds toasts = stepQuests qtext questDefs evs (lsQuests ls)

        -- NPC conversations: evaluate the dialogue script of every talked-to
        -- NPC against the current snapshot.
        env = scriptEnvOf ls { lsQuests = qlog }
        talked = [ def | EvTalkedTo nid <- evs, Just def <- [M.lookup nid npcDefs] ]
        (dlgLines, qlog', wcmds', toasts') =
          foldl (runDialogue env) ([], qlog, wcmds, toasts) talked

        hud' = pushToasts toasts' (lsHud ls)
        flow'' = openDialogue dlgLines flow'
    in ( ls { lsFlow = flow'', lsQuests = qlog', lsHud = hud' }
       , LogicOut cmds wcmds' )
  where
    runDialogue env (lns, qlog, wcmds, toasts) def =
      let acts = evalDialogue env def
          says = [ (speaker, line) | ASay speaker ls' <- acts, line <- ls' ]
          named = [ (resolveSpeaker s, l) | (s, l) <- says ]
          others = [ a | a <- acts, notSay a ]
          QuestOut qlog' wcmds' toasts' =
            foldl (flip (applyQuestAction qtext questDefs))
                  (QuestOut qlog [] []) others
      in ( lns <> named
         , qlog'
         , wcmds <> wcmds'
         , toasts <> toasts'
         )

    notSay (ASay _ _) = False
    notSay _          = True

    -- Speakers in scripts are npc ids; show their display names.
    resolveSpeaker :: Text -> Text
    resolveSpeaker s = maybe s ndName (M.lookup (NpcId s) npcDefs)
