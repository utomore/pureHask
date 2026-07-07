-- | The definition-asset pipeline: load and CROSS-VALIDATE every content
--   file (string table, items, quests, NPCs, enemies, scene files) against
--   the discovered level set, producing one immutable 'GameDefs' bundle.
--
--   Shared by three callers:
--
--     * startup           — 'Left' aborts the launch (fail loudly),
--     * hot reload        — 'Left' keeps the previous bundle and warns,
--     * @--validate@ mode — 'Left' prints and exits non-zero (CI-friendly).
module Game.Assets
  ( GameDefs(..)
  , loadDefs
  ) where

import Control.Monad (forM_, unless)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Core.Lang (LangTable, loadLang, langLookup)
import Core.Settings (Language, langCode)
import Core.Types
import Enemy.Script (EnemyDef(..), loadEnemyDir)
import Items.Registry (ItemRegistry, loadRegistry, requireItem)
import Npc.Script (NpcDef(..), loadNpcDir, npcItemRefs, npcQuestRefs)
import Quest.Runtime (QuestText(..))
import Quest.Script (QuestDef(..), loadQuestDir, questItemRefs, questEnemyRefs)
import World.Level (loadLevelFile, LevelData(..))
import World.Scene (loadSceneFile)

-- | Every compiled definition asset, as one swap-able bundle.
data GameDefs = GameDefs
  { gdLang      :: !LangTable
  , gdRegistry  :: !ItemRegistry
  , gdQuests    :: ![QuestDef]
  , gdNpcs      :: !(M.Map NpcId NpcDef)
  , gdEnemies   :: !(M.Map EnemyId EnemyDef)
  , gdQuestText :: !QuestText
  }

-- | Load and cross-validate everything. Levels themselves are inputs here
--   (they are not hot-reloadable — a level change rebuilds worlds), but
--   every reference INTO them is checked.
loadDefs :: Language -> [FilePath] -> [String] -> IO (Either String GameDefs)
loadDefs lang levelFiles levelNames = do
  langR <- loadLang ("assets/lang/" <> langCode lang <> ".lang")
  case langR of
    Left err -> pure (Left err)
    Right table -> do
      registryR <- loadRegistry table "assets/items/items.def"
      questsR <- loadQuestDir table "assets/quests"
      npcsR <- loadNpcDir table "assets/npcs"
      enemiesR <- loadEnemyDir table "assets/enemies"
      levelChecks <- mapM checkLevel levelFiles
      sceneChecks <- mapM checkScene levelNames
      pure $ do
        registry <- registryR
        quests <- questsR
        npcList <- npcsR
        enemyList <- enemiesR
        qtext <- QuestText
          <$> langLookup table "ui.quest.started"
          <*> langLookup table "ui.quest.completed"
        levels <- sequence levelChecks
        sequence_ sceneChecks
        crossValidate registry quests npcList enemyList levels
        Right GameDefs
          { gdLang = table
          , gdRegistry = registry
          , gdQuests = quests
          , gdNpcs = M.fromList [ (ndId nd, nd) | nd <- npcList ]
          , gdEnemies = M.fromList [ (edId ed, ed) | ed <- enemyList ]
          , gdQuestText = qtext
          }
  where
    checkLevel path = do
      parsed <- loadLevelFile path
      pure $ either (Left . (("level " <> path <> ": ") <>)) Right parsed

    checkScene name = do
      parsed <- loadSceneFile ("assets/scenes/" <> name <> ".scene")
      pure $ either (Left . (("scene " <> name <> ": ") <>)) (const (Right ())) parsed

-- | Every dangling reference is a hard error: quest items/enemies, npc
--   items/quests/levels, enemy spawn levels, level item markers.
crossValidate
  :: ItemRegistry -> [QuestDef] -> [NpcDef] -> [EnemyDef] -> [LevelData]
  -> Either String ()
crossValidate registry quests npcList enemyList levels = do
  let questIds = map qdId quests
      enemyIds = map edId enemyList
      knownLevel lvl = lvl `elem` map (T.pack . ldName) levels

  forM_ levels $ \lvl ->
    forM_ (ldItems lvl) $ \(_, iid) -> requireItem registry (ldName lvl) iid

  forM_ quests $ \qd -> do
    mapM_ (requireItem registry "quest") (questItemRefs qd)
    forM_ (questEnemyRefs qd) $ \e ->
      unless (e `elem` enemyIds) $
        Left ("quest '" <> show (qdId qd) <> "' references unknown enemy " <> show e)

  forM_ npcList $ \nd -> do
    mapM_ (requireItem registry "npc") (npcItemRefs nd)
    forM_ (npcQuestRefs nd) $ \q ->
      unless (q `elem` questIds) $
        Left ("npc '" <> show (ndId nd) <> "' references unknown quest " <> show q)
    unless (knownLevel (ndLevel nd)) $
      Left ("npc '" <> show (ndId nd) <> "' spawns in unknown level "
            <> show (ndLevel nd))

  forM_ enemyList $ \ed ->
    forM_ (edSpawns ed) $ \(lvl, _) ->
      unless (knownLevel lvl) $
        Left ("enemy '" <> show (edId ed) <> "' spawns in unknown level "
              <> show lvl)
