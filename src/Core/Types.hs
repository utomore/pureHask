-- | Pure vocabulary shared by every layer of the game.
--
--   This module MUST stay free of Apecs, SDL and Reflex imports. Every type
--   here is plain data that the pure state machines (Input.Semantics,
--   Flow.Machine, Sim.CombatCore) consume and produce, which is what makes
--   those machines unit-testable.
module Core.Types
  ( -- * Basic vocabulary
    Direction(..)
  , ItemId(..)
  , QuestId(..)
  , NpcId(..)
  , EnemyId(..)
  , EquipSlot(..)
  , ItemCategory(..)
    -- * Raw input (SDL-agnostic key snapshot)
  , RawInput(..)
  , emptyRawInput
    -- * Semantic input (output of the intent layer)
  , Intent(..)
  , HeldKeys(..)
  , noKeysHeld
  , FrameInput(..)
  , emptyFrameInput
    -- * Combat
  , CombatState(..)
  , HookState(..)
  , VFXType(..)
  , Vitals(..)
  , fullVitals
  , DerivedStats(..)
  , baseStats
    -- * Quests
  , QuestPhase(..)
  , QuestProgress(..)
  , QuestLog(..)
  , emptyQuestLog
    -- * Game flow
  , GameEvent(..)
  , GameMode(..)
  , RunStats(..)
  , emptyRunStats
  , FlowCommand(..)
  , WorldCommand(..)
    -- * Menu
  , MenuPage(..)
  , menuPages
  , menuPageTitle
  , MenuCursor(..)
  , initialCursor
  , MenuEnv(..)
  , emptyMenuEnv
  , WorldSnapshot(..)
  , emptyWorldSnapshot
  ) where

import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Linear (V2(..))

--------------------------------------------------------------------------------
-- Basic vocabulary
--------------------------------------------------------------------------------

-- | Horizontal facing / movement direction.
data Direction = DirLeft | DirRight deriving (Eq, Show)

-- | Identifier of an item defined in @assets/items/items.def@. Content-level
--   instances are data, validated against the 'Items.Registry.ItemRegistry'
--   at startup (dangling ids abort the launch); code-level distinctions stay
--   in the 'ItemCategory' / 'EquipSlot' sum types below. See
--   docs/SYSTEMS_DESIGN.md §2.1 for the rationale of this two-tier rule.
newtype ItemId = ItemId Text deriving (Eq, Ord, Show)

-- | Identifier of a quest defined in @assets/quests/*.quest@.
newtype QuestId = QuestId Text deriving (Eq, Ord, Show)

-- | Identifier of an NPC defined in @assets/npcs/*.npc@.
newtype NpcId = NpcId Text deriving (Eq, Ord, Show)

-- | Identifier of an enemy kind defined in @assets/enemies/*.enemy@.
newtype EnemyId = EnemyId Text deriving (Eq, Ord, Show)

-- | The five equipment slots. Fixed at the code level: equipment BEHAVIOUR
--   is logic, so the compiler owns it.
data EquipSlot = SlotWeapon | SlotBody | SlotShoes | SlotGloves | SlotHead
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Behavioural class of an item; drives backpack tabs, usability and
--   equipability. Quest items can never be dropped or consumed.
data ItemCategory
  = CatGeneral
  | CatPotion
  | CatQuest
  | CatEquip !EquipSlot
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
-- Raw input
--------------------------------------------------------------------------------

-- | A backend-agnostic snapshot of the buttons the game cares about.
--   "Main" translates SDL keycodes into this record; nothing below the
--   translation layer ever sees an SDL type.
data RawInput = RawInput
  { rawLeft      :: !Bool
  , rawRight     :: !Bool
  , rawUp        :: !Bool  -- ^ menu navigation only
  , rawDown      :: !Bool  -- ^ menu navigation only
  , rawJump      :: !Bool
  , rawAttack    :: !Bool
  , rawPick      :: !Bool
  , rawBackpack  :: !Bool
  , rawHook      :: !Bool
  , rawMenu      :: !Bool  -- ^ Escape: menu / back
  , rawUsePotion :: !Bool  -- ^ Q: drink the first potion in the backpack
  , rawInteract  :: !Bool  -- ^ E: talk / interact
  } deriving (Eq, Show)

emptyRawInput :: RawInput
emptyRawInput = RawInput
  False False False False False False False False False False False False

--------------------------------------------------------------------------------
-- Semantic input
--------------------------------------------------------------------------------

-- | A player intention derived from raw key edges by the input-semantics
--   machine. Simulation code consumes intents; it never looks at raw keys
--   except through 'HeldKeys'.
data Intent
  = IntentJump              -- ^ jump key pressed this frame
  | IntentDash !Direction   -- ^ double-tap dash detected
  | IntentAttackPress
  | IntentAttackRelease
  | IntentPickUp
  | IntentToggleBackpack
  | IntentHook
  | IntentUsePotion
  | IntentInteract          -- ^ E: talk to a nearby NPC
  | IntentMenu              -- ^ Escape pressed (menu, back, quit at title)
  | IntentNavLeft           -- ^ menu navigation (also fired while playing;
  | IntentNavRight          --   only the flow machine consumes these)
  | IntentNavUp
  | IntentNavDown
  deriving (Eq, Show)

-- | Keys currently held down; for continuous controls (running, charging).
data HeldKeys = HeldKeys
  { heldLeft   :: !Bool
  , heldRight  :: !Bool
  , heldJump   :: !Bool
  , heldAttack :: !Bool
  } deriving (Eq, Show)

noKeysHeld :: HeldKeys
noKeysHeld = HeldKeys False False False False

-- | What the simulation receives each sub-step: one-shot intents plus the
--   held-key state. Intents are delivered to the FIRST fixed sub-step of a
--   frame only; held keys apply to all sub-steps.
data FrameInput = FrameInput
  { fiIntents :: ![Intent]
  , fiHeld    :: !HeldKeys
  } deriving (Eq, Show)

emptyFrameInput :: FrameInput
emptyFrameInput = FrameInput [] noKeysHeld

--------------------------------------------------------------------------------
-- Combat
--------------------------------------------------------------------------------

-- | Combat state machine for player actions. Transitions live in
--   "Sim.CombatCore" (pure) and, for physics-driven transitions such as
--   plunge landing, in "Sim.Physics".
data CombatState
  = StateIdle        { dashCooldown   :: !Double }
  | StateDashing     { dashTimeLeft   :: !Double }
  | StateDashJump    -- ^ airborne with carried dash momentum; ends on landing
  | StateMelee       { attackTimeLeft :: !Double }
  | StateCharging    { chargeTime     :: !Double }
  | StateThrust      { thrustTimeLeft :: !Double }
  | StatePlunge
  | StateHookPulling { combatAnchor   :: !(V2 Double) }
  | StateHookHanging { combatAnchor   :: !(V2 Double) }
  deriving (Eq, Show)

-- | Grappling-hook lifecycle, independent of the combat state.
data HookState
  = HookRetracted
  | HookFlying   { hookPos :: !(V2 Double), hookVel :: !(V2 Double) }
  | HookAnchored { hookPos :: !(V2 Double) }
  deriving (Eq, Show)

-- | Visual effect flavours understood by the renderer.
data VFXType = VFXMelee | VFXThrust | VFXShockwave | VFXDashGhost
  deriving (Eq, Show)

-- | Player life values. HP hits zero → death; stamina gates mobility moves
--   (dash, double jump, hook); MP is reserved for future skills.
data Vitals = Vitals
  { vHp         :: !Double
  , vMaxHp      :: !Double
  , vMp         :: !Double
  , vMaxMp      :: !Double
  , vStamina    :: !Double
  , vMaxStamina :: !Double
  } deriving (Eq, Show)

-- | Fresh vitals at the given maxima.
fullVitals :: Double -> Double -> Double -> Vitals
fullVitals hp mp st = Vitals hp hp mp mp st st

-- | Combat-relevant totals derived from base stats plus equipment. Computed
--   by "Sim.EquipCore" whenever equipment changes and cached on the player;
--   the combat machine reads these and never knows equipment exists.
data DerivedStats = DerivedStats
  { dsAtk       :: !Int
  , dsDef       :: !Int
  , dsSpeedMult :: !Double  -- ^ movement speed multiplier (1.0 = base)
  } deriving (Eq, Show)

-- | Unequipped baseline.
baseStats :: DerivedStats
baseStats = DerivedStats 0 0 1.0

--------------------------------------------------------------------------------
-- Quests
--------------------------------------------------------------------------------

-- | Lifecycle phase of a quest.
data QuestPhase = QAvailable | QActive | QDone
  deriving (Eq, Show)

-- | Where the player stands in one quest.
data QuestProgress = QuestProgress
  { qpPhase :: !QuestPhase
  , qpStage :: !Int  -- ^ current stage index (0-based)
  , qpCount :: !Int  -- ^ progress inside a counting objective
  } deriving (Eq, Show)

-- | All quest progress plus the global script flags. Owned by
--   "Quest.Runtime" inside the Reflex network; saved verbatim.
data QuestLog = QuestLog
  { qlQuests :: !(Map QuestId QuestProgress)
  , qlFlags  :: !(Set Text)
  } deriving (Eq, Show)

emptyQuestLog :: QuestLog
emptyQuestLog = QuestLog Map.empty Set.empty

--------------------------------------------------------------------------------
-- Game flow
--------------------------------------------------------------------------------

-- | Facts produced by the simulation that the flow layer (and, later, audio
--   or achievements) reacts to. Simulation code EMITS these; it never decides
--   what happens next — that is the flow machine's job.
data GameEvent
  = EvPlayerDied
  | EvGoalReached
  | EvItemPicked !ItemId
  | EvItemUsed !ItemId
  | EvEquipChanged
  | EvTalkedTo !NpcId                        -- ^ player interacted with an NPC
  | EvEnemyKilled !EnemyId                   -- ^ an enemy dropped to 0 hp
  | EvRunRestored !Int !RunStats !QuestLog   -- ^ a save was loaded
  deriving (Eq, Show)

-- | Top-level mode of the whole application, owned by "Flow.Machine" inside
--   the Reflex network. The main loop only reads this. 'ModeMenu' freezes the
--   simulation (there is no separate pause mode; the menu IS the pause).
data GameMode
  = ModeTitle
  | ModePlaying
  | ModeMenu
  | ModeDialogue ![(Text, Text)] -- ^ pending (speaker, line) pairs; sim frozen
  | ModeDead          !Double    -- ^ respawn countdown (seconds)
  | ModeLevelComplete !Double    -- ^ transition countdown (seconds)
  | ModeEnding
  deriving (Eq, Show)

-- | Statistics of the current run, folded from 'GameEvent's by the flow layer.
data RunStats = RunStats
  { statDeaths  :: !Int
  , statItems   :: !Int
  , statTime    :: !Double -- ^ seconds spent in 'ModePlaying'
  } deriving (Eq, Show)

emptyRunStats :: RunStats
emptyRunStats = RunStats 0 0 0

-- | Instructions from the flow layer back to the effectful shell. The flow
--   machine decides WHEN these happen; "Main" executes WHAT they do.
data FlowCommand
  = CmdNewGame             -- ^ load level 0 with a fresh player
  | CmdLoadLevel !Int      -- ^ load level by index, carrying the player over
  | CmdRespawnPlayer
  | CmdUseItem !ItemId     -- ^ consume a potion picked in the menu
  | CmdEquip !ItemId       -- ^ equip a gear item picked in the menu
  | CmdUnequip !EquipSlot  -- ^ take off the gear in a slot
  | CmdSaveGame !Int       -- ^ write save slot (0 = autosave)
  | CmdLoadGame !Int       -- ^ read save slot (0 = autosave)
  | CmdToggleSetting !Int  -- ^ toggle the Nth settings row
  | CmdQuit
  deriving (Eq, Show)

-- | Instructions from the quest/script layer to the world. Like
--   'FlowCommand', the pure machines decide WHEN, the shell executes WHAT —
--   scripts never mutate the ECS directly.
data WorldCommand
  = WcGiveItem !ItemId !Int
  | WcTakeItem !ItemId !Int
  | WcSpawnNpc !NpcId !Double !Double  -- ^ tile coordinates
  | WcDespawnNpc !NpcId
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Menu
--------------------------------------------------------------------------------

-- | Pages of the in-game menu, in tab order.
data MenuPage
  = PageStatus
  | PageBackpack
  | PageEquip
  | PageQuests
  | PageMap
  | PageSave
  | PageLoad
  | PageSettings
  deriving (Eq, Show, Enum, Bounded)

menuPages :: [MenuPage]
menuPages = [minBound .. maxBound]

menuPageTitle :: MenuPage -> String
menuPageTitle p = case p of
  PageStatus   -> "STATUS"
  PageBackpack -> "BAG"
  PageEquip    -> "EQUIP"
  PageQuests   -> "QUESTS"
  PageMap      -> "MAP"
  PageSave     -> "SAVE"
  PageLoad     -> "LOAD"
  PageSettings -> "CONFIG"

-- | Where the menu cursor sits. Owned by the flow machine.
data MenuCursor = MenuCursor
  { mcPage :: !MenuPage
  , mcRow  :: !Int
  } deriving (Eq, Show)

initialCursor :: MenuCursor
initialCursor = MenuCursor PageStatus 0

-- | A pure snapshot of the world data the menu can act on, built by the
--   shell each frame and fed to the flow machine so cursor clamping and
--   confirm actions stay pure and testable.
data MenuEnv = MenuEnv
  { meBackpack  :: ![(ItemId, ItemCategory, Int)] -- ^ sorted rows (id, cat, count)
  , meEquipped  :: ![(EquipSlot, Maybe ItemId)]   -- ^ the five slots
  , meSaveSlots :: ![Maybe String]                -- ^ manual slot summaries
  , meAutoSave  :: !(Maybe String)                -- ^ autosave summary
  , meSettings  :: ![(String, String)]            -- ^ settings rows (label, value)
  } deriving (Eq, Show)

emptyMenuEnv :: MenuEnv
emptyMenuEnv = MenuEnv [] [] [] Nothing []

-- | A pure per-frame snapshot of the world facts the script layer may read
--   (dialogue conditions such as @(has-item …)@ or @(player-near …)@).
--   Built by the shell, carried in 'Flow.Machine.FlowFrame'.
data WorldSnapshot = WorldSnapshot
  { wsBackpack  :: !(Map ItemId Int)
  , wsPlayerPos :: !(V2 Double)  -- ^ player center, pixels
  , wsLevelName :: !Text
  } deriving (Eq, Show)

emptyWorldSnapshot :: WorldSnapshot
emptyWorldSnapshot = WorldSnapshot Map.empty (V2 0 0) ""
