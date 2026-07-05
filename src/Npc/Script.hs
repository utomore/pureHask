-- | NPC definitions: compiling @assets/npcs/*.npc@ files.
--
--   Format:
--
--   > (npc elder
--   >   (name "ELDER")
--   >   (color 200 180 120)
--   >   (size 24 32)
--   >   (spawn-at 01-training 22 13)           ; level name, tile x, tile y
--   >   (movement (patrol 20 24) (speed 40) (pause 1.5))  ; or (movement (idle))
--   >   (chatter 8 "MY BACK ACHES" "COLD DOWN HERE")      ; optional bubbles
--   >   (dialogue
--   >     (rule (quest-state find-key done) (say elder "WELL DONE."))
--   >     (default (say elder "WELCOME.") (set-flag met-elder))))
--
--   Dialogue rules are tried TOP TO BOTTOM; the first whose condition holds
--   wins; @(default …)@ always matches. Personality = movement parameters +
--   chatter lines; it lives entirely in this data.
--   Adding an NPC = adding a file. No recompilation.
module Npc.Script
  ( NpcDef(..)
  , NpcMovement(..)
  , DialogueRule(..)
  , compileNpc
  , loadNpcDir
  , npcItemRefs
  , npcQuestRefs
  ) where

import qualified Data.ByteString as BS
import Data.List (sort, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8')
import Linear (V2(..))
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))

import Core.Config (tileSize)
import Core.Types (ItemId, QuestId, NpcId(..))
import Script.Expr
import Script.Sexp

-- | How an NPC moves. Patrol coordinates are in pixels (converted from the
--   tile coordinates in the file).
data NpcMovement
  = MoveIdle
  | MovePatrol { mpMinX :: !Double, mpMaxX :: !Double
               , mpSpeed :: !Double, mpPause :: !Double }
  deriving (Eq, Show)

-- | One dialogue rule: 'Nothing' condition = the @(default …)@ rule.
data DialogueRule = DialogueRule
  { drCond    :: !(Maybe Cond)
  , drActions :: ![Action]
  } deriving (Eq, Show)

data NpcDef = NpcDef
  { ndId       :: !NpcId
  , ndName     :: !Text
  , ndColor    :: !(Int, Int, Int)
  , ndSize     :: !(V2 Double)
  , ndLevel    :: !Text           -- ^ level (file base name) it spawns in
  , ndSpawn    :: !(V2 Double)    -- ^ pixels
  , ndMovement :: !NpcMovement
  , ndChatter  :: !(Maybe (Double, [Text]))  -- ^ interval, cycled lines
  , ndDialogue :: ![DialogueRule]
  } deriving (Eq, Show)

compileNpc :: [Sexp] -> Either String NpcDef
compileNpc [] = Left "npc: (npc …) without an id"
compileNpc (idForm : body) = do
  rawId <- maybe (Left "npc: id must be a symbol") Right (sexpSymbol idForm)
  let ctx = "npc '" <> T.unpack rawId <> "'"

  name <- require ctx "name" $ fieldOf "name" body >>= safeHead >>= sexpString

  color <- case fieldOf "color" body of
    Just [SNum r, SNum g, SNum b] -> Right (round r, round g, round b)
    Just other -> Left (ctx <> ": bad color " <> show other)
    Nothing    -> Right (200, 180, 120)

  size <- case fieldOf "size" body of
    Just [SNum w, SNum h] -> Right (V2 w h)
    Just other -> Left (ctx <> ": bad size " <> show other)
    Nothing    -> Right (V2 24.0 32.0)

  (level, spawn) <- case fieldOf "spawn-at" body of
    Just [SSym lvl, SNum tx, SNum ty] ->
      Right (lvl, V2 (tx * tileSize) (ty * tileSize))
    Just other -> Left (ctx <> ": bad spawn-at " <> show other)
    Nothing    -> Left (ctx <> ": missing (spawn-at LEVEL X Y)")

  movement <- compileMovement ctx (fieldOf "movement" body) spawn

  chatter <- case fieldOf "chatter" body of
    Nothing -> Right Nothing
    Just (SNum interval : rest)
      | Just lns <- mapM sexpString rest, not (null lns) ->
          Right (Just (interval, lns))
    Just other -> Left (ctx <> ": bad chatter " <> show other)

  dialogue <- case fieldOf "dialogue" body of
    Nothing    -> Right []
    Just forms -> mapM (compileRule ctx) forms

  Right NpcDef
    { ndId = NpcId rawId, ndName = name, ndColor = color, ndSize = size
    , ndLevel = level, ndSpawn = spawn, ndMovement = movement
    , ndChatter = chatter, ndDialogue = dialogue
    }
  where
    safeHead (x : _) = Just x
    safeHead []      = Nothing
    require ctx' what = maybe (Left (ctx' <> ": missing (" <> what <> " …)")) Right

compileMovement :: String -> Maybe [Sexp] -> V2 Double -> Either String NpcMovement
compileMovement _ Nothing _ = Right MoveIdle
compileMovement ctx (Just parts) _spawn = do
  let get name = fieldOf name parts
  case get "idle" of
    Just _ -> Right MoveIdle
    Nothing -> case get "patrol" of
      Just [SNum x1, SNum x2] -> do
        speed <- case get "speed" of
          Just [SNum s] -> Right s
          Nothing       -> Right 40.0
          Just other    -> Left (ctx <> ": bad speed " <> show other)
        pause <- case get "pause" of
          Just [SNum p] -> Right p
          Nothing       -> Right 1.0
          Just other    -> Left (ctx <> ": bad pause " <> show other)
        Right (MovePatrol (min x1 x2 * tileSize) (max x1 x2 * tileSize) speed pause)
      Just other -> Left (ctx <> ": bad patrol " <> show other)
      Nothing    -> Left (ctx <> ": movement needs (idle) or (patrol X1 X2)")

compileRule :: String -> Sexp -> Either String DialogueRule
compileRule ctx form = case form of
  SList (SSym "rule" : condForm : actionForms)
    | not (null actionForms) -> do
        c <- either (Left . ((ctx <> ": ") <>)) Right (compileCond condForm)
        acts <- mapM (either (Left . ((ctx <> ": ") <>)) Right . compileAction)
                  actionForms
        Right (DialogueRule (Just c) acts)
  SList (SSym "default" : actionForms)
    | not (null actionForms) -> do
        acts <- mapM (either (Left . ((ctx <> ": ") <>)) Right . compileAction)
                  actionForms
        Right (DialogueRule Nothing acts)
  _ -> Left (ctx <> ": bad dialogue rule " <> show form)

-- | Item ids referenced by dialogue (for startup validation).
npcItemRefs :: NpcDef -> [ItemId]
npcItemRefs nd = concat
  [ maybe [] condItemRefs (drCond r) <> concatMap actionItemRefs (drActions r)
  | r <- ndDialogue nd
  ]

-- | Quest ids referenced by dialogue.
npcQuestRefs :: NpcDef -> [QuestId]
npcQuestRefs nd = concat
  [ maybe [] condQuestRefs (drCond r) <> concatMap actionQuestRefs (drActions r)
  | r <- ndDialogue nd
  ]

-- | Load and compile every @*.npc@ file in a directory (sorted by name).
loadNpcDir :: FilePath -> IO (Either String [NpcDef])
loadNpcDir dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then pure (Right [])
    else do
      entries <- listDirectory dir
      let files = [ dir </> e | e <- sort entries, ".npc" `isSuffixOf` e ]
      results <- mapM loadOne files
      pure (concat <$> sequence results)
  where
    loadOne path = do
      bytes <- BS.readFile path
      pure $ case decodeUtf8' bytes of
        Left err -> Left (path <> ": not valid UTF-8 (" <> show err <> ")")
        Right txt -> do
          forms <- either (Left . ((path <> ": ") <>)) Right (parseSexps txt)
          mapM (either (Left . ((path <> ": ") <>)) Right . compileNpc)
               (formsNamed "npc" forms)
