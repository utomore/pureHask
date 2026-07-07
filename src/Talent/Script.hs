-- | Talent definitions: compiling @assets/talents/talents.def@.
--
--   Format (one @(talent …)@ form per node; file order = display order):
--
--   > (talent blade-1
--   >   (name talent.blade-1.name)      ; display name (string or lang key)
--   >   (desc talent.blade-1.desc)
--   >   (max-rank 3)                    ; how many times it can be bought
--   >   (cost 1)                        ; talent points per rank
--   >   (requires blade-0 2)            ; optional: EARLIER node at >= rank
--   >   (effect (atk 2)))               ; additive bonuses PER RANK
--
--   Effects vocabulary (code-level sum type, compiler-checked):
--   @(atk N) (def N) (spd N) (max-hp N) (max-stamina N)@ — @spd@ is a
--   movement-speed bonus in percent, like equipment.
--
--   @requires@ must name a talent defined EARLIER in the file. That single
--   rule makes the graph acyclic by construction and keeps "walk the tree"
--   logic trivial. Adding a talent = editing the file. No recompilation.
module Talent.Script
  ( TalentDef(..)
  , TalentEffect(..)
  , compileTalents
  , loadTalentFile
  ) where

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import System.Directory (doesFileExist)

import Core.Lang (LangTable, langText)
import Core.Types (TalentId(..))
import Script.Sexp

-- | One additive stat bonus, applied once per learned rank.
data TalentEffect
  = EffAtk !Int
  | EffDef !Int
  | EffSpdPct !Int
  | EffMaxHp !Int
  | EffMaxStamina !Int
  deriving (Eq, Show)

-- | One node of the talent tree.
data TalentDef = TalentDef
  { tdId       :: !TalentId
  , tdName     :: !Text
  , tdDesc     :: !Text
  , tdMaxRank  :: !Int
  , tdCost     :: !Int                       -- ^ points per rank
  , tdRequires :: !(Maybe (TalentId, Int))   -- ^ prerequisite node and rank
  , tdEffects  :: ![TalentEffect]            -- ^ per-rank bonuses
  } deriving (Eq, Show)

-- | Compile every @(talent …)@ form. Duplicate ids, unknown effects and
--   forward/self @requires@ references are hard errors.
compileTalents :: LangTable -> [Sexp] -> Either String [TalentDef]
compileTalents table forms = do
  defs <- go [] (formsNamed "talent" forms)
  pure (reverse defs)
  where
    go done [] = Right done
    go done (body : rest) = do
      def <- compileTalent table (map tdId done) body
      if tdId def `elem` map tdId done
        then let TalentId raw = tdId def
             in Left ("talents.def: duplicate talent id '" <> T.unpack raw <> "'")
        else go (def : done) rest

compileTalent :: LangTable -> [TalentId] -> [Sexp] -> Either String TalentDef
compileTalent _ _ [] = Left "talents.def: (talent …) without an id"
compileTalent table earlier (idForm : body) = do
  rawId <- maybe (Left "talents.def: talent id must be a symbol") Right
             (sexpSymbol idForm)
  let ctx = "talent '" <> T.unpack rawId <> "'"
      textOf form = either (Left . ((ctx <> ": ") <>)) Right (langText table form)

  name <- case fieldOf "name" body of
    Just [form] -> textOf form
    _           -> Left (ctx <> ": missing (name …)")

  desc <- case fieldOf "desc" body of
    Just [form] -> textOf form
    Nothing     -> Right ""
    Just other  -> Left (ctx <> ": bad desc " <> show other)

  maxRank <- case fieldOf "max-rank" body of
    Just [SNum n] | n >= 1 -> Right (round n)
    Nothing                -> Right 1
    Just other             -> Left (ctx <> ": bad max-rank " <> show other)

  cost <- case fieldOf "cost" body of
    Just [SNum n] | n >= 1 -> Right (round n)
    Nothing                -> Right 1
    Just other             -> Left (ctx <> ": bad cost " <> show other)

  requires <- case fieldOf "requires" body of
    Nothing -> Right Nothing
    Just [SSym parent] -> requireEarlier ctx parent 1
    Just [SSym parent, SNum r] | r >= 1 -> requireEarlier ctx parent (round r)
    Just other -> Left (ctx <> ": bad requires " <> show other)

  effects <- case fieldOf "effect" body of
    Just parts | not (null parts) -> mapM (parseEffect ctx) parts
    _ -> Left (ctx <> ": missing (effect …)")

  Right TalentDef
    { tdId = TalentId rawId, tdName = name, tdDesc = desc
    , tdMaxRank = maxRank, tdCost = cost
    , tdRequires = requires, tdEffects = effects
    }
  where
    requireEarlier ctx parent rank
      | TalentId parent `elem` earlier = Right (Just (TalentId parent, rank))
      | otherwise = Left (ctx <> ": (requires " <> T.unpack parent
                          <> " …) must name a talent defined EARLIER in the file")

parseEffect :: String -> Sexp -> Either String TalentEffect
parseEffect ctx form = case form of
  SList [SSym "atk", SNum n]         -> Right (EffAtk (round n))
  SList [SSym "def", SNum n]         -> Right (EffDef (round n))
  SList [SSym "spd", SNum n]         -> Right (EffSpdPct (round n))
  SList [SSym "max-hp", SNum n]      -> Right (EffMaxHp (round n))
  SList [SSym "max-stamina", SNum n] -> Right (EffMaxStamina (round n))
  other -> Left (ctx <> ": unknown effect " <> show other)

-- | Load and compile the talent tree. A missing file means "no talents"
--   (the menu page simply shows an empty tree) — projects without talents
--   should not be forced to create one.
loadTalentFile :: LangTable -> FilePath -> IO (Either String [TalentDef])
loadTalentFile table path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right [])
    else do
      bytes <- BS.readFile path
      pure $ case decodeUtf8' bytes of
        Left err  -> Left (path <> ": not valid UTF-8 (" <> show err <> ")")
        Right txt ->
          either (Left . ((path <> ": ") <>)) Right (parseSexps (stripBom txt))
            >>= compileTalents table
  where
    stripBom t = maybe t id (T.stripPrefix "\65279" t)
