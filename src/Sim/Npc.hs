-- | Apecs glue for NPCs: spawning per level, stepping the pure brains,
--   executing NPC world commands, and the talk interaction.
module Sim.Npc
  ( spawnNpcsForLevel
  , stepNpcs
  , runNpcCommand
  , interactNearest
  ) where

import Apecs
import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Linear (V2(..), norm)

import Core.Components
import Core.Config
import Core.Types
import Npc.Core
import Npc.Script
import Sim.Rules (emitEvent)

-- | Talk range in pixels.
interactRange :: Double
interactRange = 56.0

npcComponents :: Proxy (Npc, NpcBrain, Position, Velocity, Collider, Gravity, IsGrounded)
npcComponents = Proxy

-- | Spawn every NPC that belongs in the given level.
spawnNpcsForLevel :: M.Map NpcId NpcDef -> Text -> Game ()
spawnNpcsForLevel defs levelName =
  forM_ [ d | d <- M.elems defs, ndLevel d == levelName ] spawnOne

spawnOne :: NpcDef -> Game ()
spawnOne def = do
  _ <- newEntity ( Npc (ndId def)
                 , NpcBrain (initialAi def)
                 , Position (ndSpawn def)
                 , Velocity (V2 0.0 0.0)
                 , Collider (ndSize def)
                 , Gravity gravityAccel
                 , IsGrounded False
                 )
  return ()

-- | One sub-step of every NPC brain: AI decides the horizontal velocity, the
--   shared physics system moves the body.
stepNpcs :: M.Map NpcId NpcDef -> Double -> Game ()
stepNpcs defs dt =
  cmapM $ \(Npc nid, NpcBrain ai, Position pos, Velocity (V2 _ vy)) ->
    case M.lookup nid defs of
      Nothing -> return (NpcBrain ai, Velocity (V2 0.0 vy))
      Just def ->
        let (ai', vx) = stepNpcAi def dt pos ai
        in return (NpcBrain ai', Velocity (V2 vx vy))

-- | Execute NPC-related world commands (spawn/despawn by id).
runNpcCommand :: M.Map NpcId NpcDef -> WorldCommand -> Game ()
runNpcCommand defs wc = case wc of
  WcSpawnNpc nid tx ty ->
    case M.lookup nid defs of
      Nothing  -> return ()  -- validated at startup; cannot happen
      Just def -> spawnOne def { ndSpawn = V2 (tx * tileSize) (ty * tileSize) }
  WcDespawnNpc nid ->
    cmapM_ $ \(Npc other, ety) ->
      when (other == nid) $ destroy ety npcComponents
  _ -> return ()

-- | The E key: talk to the nearest NPC in range. Emits 'EvTalkedTo'; the
--   logic layer evaluates the dialogue script.
interactNearest :: Game ()
interactNearest =
  cmapM_ $ \(Player, Position ppos, Collider psize) -> do
    let playerCenter = ppos + psize / 2.0
    candidates <- cfold
      (\acc (Npc nid, Position npos, Collider nsize) ->
         let d = norm (npos + nsize / 2.0 - playerCenter)
         in if d <= interactRange then (d, nid) : acc else acc)
      []
    case candidates of
      [] -> return ()
      xs -> emitEvent (EvTalkedTo (snd (minimum xs)))
