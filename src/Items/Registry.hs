-- | The item database: definitions loaded from @assets/items/items.def@ at
--   startup, immutable afterwards. Content iteration (new potions, new
--   equipment) never touches the compiler; the registry validates everything
--   at launch and fails loudly on any mistake.
--
--   File format (see docs/SYSTEMS_DESIGN.md §2):
--
--   > (item gold-key
--   >   (name "GOLD KEY")            ; display name (3x5 font: A-Z 0-9 only)
--   >   (category quest)             ; general | potion | quest | equip SLOT
--   >   (color 255 220 0)            ; world/backpack colour
--   >   (desc "OPENS THE GATE"))
--   > (item potion-hp-s
--   >   (name "SMALL POTION") (category potion) (stack 9)
--   >   (color 220 40 40) (use (heal 30)) (desc "RESTORES 30 HP"))
--   > (item sword-rusty
--   >   (name "RUSTY SWORD") (category equip weapon)
--   >   (color 180 190 200) (stats (atk 5)) (desc "SEEN BETTER DAYS"))
module Items.Registry
  ( ItemDef(..)
  , ItemStats(..)
  , UseEffect(..)
  , ItemRegistry
  , emptyStats
  , compileRegistry
  , loadRegistry
  , lookupItem
  , requireItem
  , allItems
  , itemDisplayName
  ) where

import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')

import Core.Types (ItemId(..), ItemCategory(..), EquipSlot(..))
import Script.Sexp

-- | Flat additive stat block carried by equipment.
data ItemStats = ItemStats
  { statAtk    :: !Int
  , statDef    :: !Int
  , statSpdPct :: !Int  -- ^ movement speed bonus in percent
  } deriving (Eq, Show)

emptyStats :: ItemStats
emptyStats = ItemStats 0 0 0

-- | What using (consuming) an item does. Interpreted by "Sim.Items".
data UseEffect
  = Heal !Double
  | RestoreMp !Double
  | RestoreStamina !Double
  deriving (Eq, Show)

-- | One item definition.
data ItemDef = ItemDef
  { defId       :: !ItemId
  , defName     :: !Text
  , defCategory :: !ItemCategory
  , defStack    :: !Int            -- ^ stack limit (1 = unstackable)
  , defColor    :: !(Int, Int, Int)
  , defDesc     :: !Text
  , defUse      :: ![UseEffect]    -- ^ empty = not consumable
  , defStats    :: !ItemStats      -- ^ zero for non-equipment
  } deriving (Eq, Show)

newtype ItemRegistry = ItemRegistry (Map ItemId ItemDef)
  deriving (Eq, Show)

lookupItem :: ItemRegistry -> ItemId -> Maybe ItemDef
lookupItem (ItemRegistry m) iid = M.lookup iid m

-- | Validation-flavoured lookup: for startup cross-checks of scripts/levels.
requireItem :: ItemRegistry -> String -> ItemId -> Either String ItemDef
requireItem reg context iid@(ItemId raw) =
  case lookupItem reg iid of
    Just def -> Right def
    Nothing  -> Left (context <> ": unknown item id '" <> T.unpack raw <> "'")

allItems :: ItemRegistry -> [ItemDef]
allItems (ItemRegistry m) = M.elems m

-- | Display name with a safe fallback (should not happen post-validation).
itemDisplayName :: ItemRegistry -> ItemId -> Text
itemDisplayName reg iid@(ItemId raw) =
  maybe raw defName (lookupItem reg iid)

--------------------------------------------------------------------------------
-- Compilation
--------------------------------------------------------------------------------

-- | Compile parsed forms into a registry. Duplicate ids, unknown categories
--   or malformed fields are hard errors.
compileRegistry :: [Sexp] -> Either String ItemRegistry
compileRegistry forms = do
  defs <- mapM compileItem (formsNamed "item" forms)
  let dups = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [ (defId d, 1) | d <- defs ]))
  case dups of
    (ItemId d : _) -> Left ("items.def: duplicate item id '" <> T.unpack d <> "'")
    [] -> Right (ItemRegistry (M.fromList [ (defId d, d) | d <- defs ]))

compileItem :: [Sexp] -> Either String ItemDef
compileItem [] = Left "items.def: (item …) without an id"
compileItem (idForm : body) = do
  rawId <- maybe (Left "items.def: item id must be a symbol") Right (sexpSymbol idForm)
  let ctx = "item '" <> T.unpack rawId <> "'"
      field name = fieldOf name body

  name <- case field "name" >>= safeHead >>= sexpString of
    Just n  -> Right n
    Nothing -> Left (ctx <> ": missing (name \"…\")")

  category <- case field "category" of
    Just [SSym "general"] -> Right CatGeneral
    Just [SSym "potion"]  -> Right CatPotion
    Just [SSym "quest"]   -> Right CatQuest
    Just [SSym "equip", SSym slot] -> CatEquip <$> parseSlot ctx slot
    Just other -> Left (ctx <> ": bad category " <> show other)
    Nothing    -> Left (ctx <> ": missing (category …)")

  color <- case field "color" of
    Just [SNum r, SNum g, SNum b] -> Right (round r, round g, round b)
    Just other -> Left (ctx <> ": bad color " <> show other)
    Nothing    -> Right (200, 200, 200)

  stack <- case field "stack" of
    Just [SNum n] | n >= 1 -> Right (round n)
    Just other             -> Left (ctx <> ": bad stack " <> show other)
    Nothing                -> Right 1

  uses <- case field "use" of
    Nothing      -> Right []
    Just effects -> mapM (parseUse ctx) effects

  stats <- case field "stats" of
    Nothing    -> Right emptyStats
    Just parts -> foldM' (parseStat ctx) emptyStats parts

  let desc = maybe "" id (field "desc" >>= safeHead >>= sexpString)

  Right ItemDef
    { defId = ItemId rawId, defName = name, defCategory = category
    , defStack = stack, defColor = color, defDesc = desc
    , defUse = uses, defStats = stats
    }
  where
    safeHead (x : _) = Just x
    safeHead []      = Nothing
    foldM' f z = foldl (\acc x -> acc >>= \a -> f a x) (Right z)

parseSlot :: String -> Text -> Either String EquipSlot
parseSlot ctx slot = case slot of
  "weapon" -> Right SlotWeapon
  "body"   -> Right SlotBody
  "shoes"  -> Right SlotShoes
  "gloves" -> Right SlotGloves
  "head"   -> Right SlotHead
  other    -> Left (ctx <> ": unknown equip slot '" <> T.unpack other <> "'")

parseUse :: String -> Sexp -> Either String UseEffect
parseUse ctx form = case form of
  SList [SSym "heal", SNum n]            -> Right (Heal n)
  SList [SSym "restore-mp", SNum n]      -> Right (RestoreMp n)
  SList [SSym "restore-stamina", SNum n] -> Right (RestoreStamina n)
  other -> Left (ctx <> ": unknown use effect " <> show other)

parseStat :: String -> ItemStats -> Sexp -> Either String ItemStats
parseStat ctx st form = case form of
  SList [SSym "atk", SNum n] -> Right st { statAtk = round n }
  SList [SSym "def", SNum n] -> Right st { statDef = round n }
  SList [SSym "spd", SNum n] -> Right st { statSpdPct = round n }
  other -> Left (ctx <> ": unknown stat " <> show other)

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

-- | Load and compile the item database (UTF-8, BOM-agnostic on Windows).
loadRegistry :: FilePath -> IO (Either String ItemRegistry)
loadRegistry path = do
  bytes <- BS.readFile path
  pure $ case decodeUtf8' bytes of
    Left err  -> Left (path <> ": not valid UTF-8 (" <> show err <> ")")
    Right txt -> parseSexps (stripBom txt) >>= compileRegistry
  where
    stripBom t = maybe t id (T.stripPrefix "\65279" t)
