-- | Game rules: item pickup, UI toggles and win/loss detection.
--
--   Rules OBSERVE the world and EMIT 'GameEvent's; they never change the game
--   mode themselves — that is the flow machine's job ("Flow.Machine"). This
--   one-way street (sim emits facts, flow decides consequences, shell
--   executes commands) is the core decoupling of the architecture.
module Sim.Rules
  ( emitEvent
  , drainEvents
  , applyIntents
  , useItemById
  , equipById
  , unequipBySlot
  , runWorldCommand
  , checkRules
  , aabbOverlap
  ) where

import Apecs
import Control.Monad (when)
import qualified Data.Map.Strict as M
import Linear (V2(..))

import Core.Components
import Core.Config (tileSize)
import Core.Types
import Items.Registry (ItemRegistry, defId, defUse, defCategory, lookupItem)
import Sim.EquipCore (equipItem, unequipSlot, computeStats)
import Sim.Items (applyEffects, firstPotion, consumeOne)

-- | Append a simulation fact to the global outbox.
emitEvent :: GameEvent -> Game ()
emitEvent ev = do
  EventQueue evs <- get global
  set global (EventQueue (evs ++ [ev]))

-- | Read and clear the outbox. Called once per frame by "Main" after all
--   fixed sub-steps ran.
drainEvents :: Game [GameEvent]
drainEvents = do
  EventQueue evs <- get global
  set global (EventQueue [])
  return evs

-- | AABB intersection test.
aabbOverlap :: V2 Double -> V2 Double -> V2 Double -> V2 Double -> Bool
aabbOverlap (V2 x1 y1) (V2 w1 h1) (V2 x2 y2) (V2 w2 h2) =
  x1 < x2 + w2 && x1 + w1 > x2 && y1 < y2 + h2 && y1 + h1 > y2

-- | World-affecting intents that are not combat moves: picking up items,
--   toggling the backpack overlay, quick-using a potion.
applyIntents :: ItemRegistry -> FrameInput -> Game ()
applyIntents registry input = do
  when (IntentToggleBackpack `elem` fiIntents input) $ do
    UIState shown <- get global
    set global (UIState (not shown))

  when (IntentUsePotion `elem` fiIntents input) $
    cmapM_ $ \(Player, Backpack items, playerEty) ->
      case firstPotion registry items of
        Nothing -> return ()
        Just potion -> do
          vitals :: Vitals <- get playerEty
          set playerEty ( Backpack (consumeOne (defId potion) items)
                        , applyEffects (defUse potion) vitals
                        )
          emitEvent (EvItemUsed (defId potion))

  when (IntentPickUp `elem` fiIntents input) $
    cmapM_ $ \(Player, Position pos, Collider size, playerEty) -> do
      picked <- cfoldM
        (\acc (Item item, Position itemPos, Collider itemSize, itemEty) ->
          if aabbOverlap pos size itemPos itemSize
            then do
              destroy itemEty (Proxy @(Position, Collider, Item))
              emitEvent (EvItemPicked item)
              return (item : acc)
            else return acc)
        []
      when (not (null picked)) $ do
        Backpack items <- get playerEty
        let items' = foldl (\m i -> M.insertWith (+) i 1 m) items picked
        set playerEty (Backpack items')

-- | Consume one unit of a specific potion (picked in the menu). Ignores ids
--   that are absent, not potions, or not consumable.
useItemById :: ItemRegistry -> ItemId -> Game ()
useItemById registry iid =
  cmapM_ $ \(Player, Backpack items, playerEty) ->
    case (M.lookup iid items, lookupItem registry iid) of
      (Just n, Just def)
        | n > 0 && defCategory def == CatPotion -> do
            vitals :: Vitals <- get playerEty
            set playerEty ( Backpack (consumeOne (defId def) items)
                          , applyEffects (defUse def) vitals
                          )
            emitEvent (EvItemUsed (defId def))
      _ -> return ()

-- | Equip a gear item from the backpack (menu confirm). No-op for unknown or
--   non-equipment ids. Refreshes the derived-stats cache.
equipById :: ItemRegistry -> ItemId -> Game ()
equipById registry iid =
  cmapM_ $ \(Player, Backpack items, playerEty) -> do
    Equipped equipped <- get playerEty
    case lookupItem registry iid >>= \def -> equipItem def equipped items of
      Nothing -> return ()
      Just (equipped', items') -> do
        set playerEty ( Equipped equipped'
                      , Backpack items'
                      , StatsCache (computeStats registry equipped')
                      )
        emitEvent EvEquipChanged

-- | Take the gear in a slot off, back into the backpack (menu confirm).
unequipBySlot :: ItemRegistry -> EquipSlot -> Game ()
unequipBySlot registry slot =
  cmapM_ $ \(Player, Backpack items, playerEty) -> do
    Equipped equipped <- get playerEty
    let (equipped', items') = unequipSlot slot equipped items
    when (equipped' /= equipped) $ do
      set playerEty ( Equipped equipped'
                    , Backpack items'
                    , StatsCache (computeStats registry equipped')
                    )
      emitEvent EvEquipChanged

-- | Execute a script-layer instruction against the world. NPC spawning is
--   handled by the NPC system (its commands are dispatched in "Main").
runWorldCommand :: WorldCommand -> Game ()
runWorldCommand wc = case wc of
  WcGiveItem iid n ->
    cmapM_ $ \(Player, Backpack items, playerEty) ->
      set playerEty (Backpack (M.insertWith (+) iid n items))
  WcTakeItem iid n ->
    cmapM_ $ \(Player, Backpack items, playerEty) ->
      set playerEty (Backpack (M.update
        (\have -> if have <= n then Nothing else Just (have - n)) iid items))
  WcSpawnNpc {}  -> return ()  -- NPC system (S8)
  WcDespawnNpc _ -> return ()  -- NPC system (S8)

-- | Emit death / goal events. The flow machine reacts to them on the next
--   network firing; the simulation itself keeps running unchanged.
checkRules :: V2 Double -> Double -> Game ()
checkRules goalPos mapHeightLimit =
  cmapM_ $ \(Player, Position pos, Collider size) -> do
    let V2 _ py = pos
    when (py > mapHeightLimit) $ emitEvent EvPlayerDied
    when (aabbOverlap pos size goalPos (V2 tileSize tileSize)) $
      emitEvent EvGoalReached
