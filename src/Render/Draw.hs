-- | Scene rendering: camera, tiles, entities, VFX. Reads the ECS world,
--   never writes to it. UI overlays live in "Render.UI".
module Render.Draw
  ( renderScene
  , cameraOffset
  , toSDLRect
  , itemColors
  ) where

import Control.Monad (when, forM_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.C.Types (CInt)
import Apecs hiding (($=))
import SDL (($=))
import qualified SDL
import Linear (V2(..), V4(..), norm)

import Core.Components
import Core.Config
import Core.Types
import Enemy.Script (EnemyDef(..))
import Items.Registry (ItemRegistry, lookupItem, defColor)
import Npc.Core (NpcAi(..))
import Npc.Script (NpcDef(..))
import Render.Font (FontSet(..), drawText, textWidth)
import Render.Layers (renderLayerSlots)
import Sim.ParticleCore (Particle(..))
import World.Scene (SceneDef, backSlots, frontSlots)
import World.Tilemap

-- | Convert game coordinates to an SDL rectangle.
toSDLRect :: V2 Double -> V2 Double -> SDL.Rectangle CInt
toSDLRect (V2 x y) (V2 w h) =
  SDL.Rectangle (SDL.P (V2 (round x) (round y))) (V2 (round w) (round h))

-- | Camera top-left in world coordinates: centered on the player, clamped to
--   the map bounds.
cameraOffset :: Tilemap -> V2 Double -> V2 Double
cameraOffset tilemap (V2 px py) = V2 camX camY
  where
    camX = max 0.0 (min (mapPixelWidth tilemap - screenWidth) (px - screenWidth / 2.0))
    camY = max 0.0 (min (mapPixelHeight tilemap - screenHeight) (py - screenHeight / 2.0))

-- | Draw the full game scene: back parallax layers, tiles, entities, VFX,
--   player, then the front parallax layers (see "World.Scene").
renderScene :: FontSet -> ItemRegistry -> M.Map NpcId NpcDef
            -> M.Map EnemyId EnemyDef -> Tilemap -> SceneDef -> Game ()
renderScene fonts registry npcDefs enemyDefs tilemap scene = do
  let renderer = fsRenderer fonts
  -- Camera follows the player.
  playerPositions <- cfold (\acc (Player, Position p) -> p : acc) []
  let playerPos = case playerPositions of
        (p : _) -> p
        []      -> V2 0.0 0.0
      camOffset@(V2 camX camY) = cameraOffset tilemap playerPos

  -- Background colour, then the three back parallax layers.
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  SDL.rendererDrawColor renderer $= V4 18 24 38 255
  SDL.clear renderer
  liftIO $ renderLayerSlots renderer camOffset scene backSlots

  -- Tiles (visible range only).
  let startCol = max 0 (floor (camX / tileSize))
      endCol   = min (mapWidth tilemap - 1) (ceiling ((camX + screenWidth) / tileSize))
      startRow = max 0 (floor (camY / tileSize))
      endRow   = min (mapHeight tilemap - 1) (ceiling ((camY + screenHeight) / tileSize))

  liftIO $ forM_ [startCol .. endCol] $ \col ->
    forM_ [startRow .. endRow] $ \row ->
      when (isTileSolid tilemap col row) $ do
        let tx = fromIntegral col * tileSize - camX
            ty = fromIntegral row * tileSize - camY
            rect = toSDLRect (V2 tx ty) (V2 tileSize tileSize)
        SDL.rendererDrawColor renderer $= V4 43 55 77 255
        SDL.fillRect renderer (Just rect)
        SDL.rendererDrawColor renderer $= V4 58 74 102 255
        SDL.drawRect renderer (Just rect)

  -- Goal (magenta).
  cfoldM_ (\_ (Goal, Position pos, Collider size) -> do
    let rect = toSDLRect (pos - camOffset) size
    SDL.rendererDrawColor renderer $= V4 220 50 180 255
    SDL.fillRect renderer (Just rect)
    SDL.rendererDrawColor renderer $= V4 255 100 220 255
    SDL.drawRect renderer (Just rect)
    ) ()

  -- Items on the ground (colour comes from the item registry).
  cfoldM_ (\_ (Item item, Position pos, Collider size) -> do
    let rect = toSDLRect (pos - camOffset) size
        (fill, border) = itemColors registry item
    SDL.rendererDrawColor renderer $= fill
    SDL.fillRect renderer (Just rect)
    SDL.rendererDrawColor renderer $= border
    SDL.drawRect renderer (Just rect)
    ) ()

  -- Visual effects.
  cfoldM_ (\_ (VFX life vfxType mDir, Position pos) -> do
    let relativePos = pos - camOffset
    case vfxType of
      VFXDashGhost -> do
        let rect = toSDLRect relativePos (V2 playerSize playerSize)
            alpha = round (max 0.0 (min 255.0 (life / 0.15 * 100.0)))
        SDL.rendererDrawColor renderer $= V4 0 220 220 alpha
        SDL.fillRect renderer (Just rect)

      VFXMelee -> case mDir of
        Just dir -> do
          let swipeOffset = if dir == DirLeft then V2 (-26.0) 0.0 else V2 18.0 0.0
              swipeRect = toSDLRect (relativePos + swipeOffset) (V2 32.0 24.0)
          SDL.rendererDrawColor renderer $= V4 255 170 0 160
          SDL.fillRect renderer (Just swipeRect)
          SDL.rendererDrawColor renderer $= V4 255 220 100 220
          SDL.drawRect renderer (Just swipeRect)
        Nothing -> do
          let flashRect = toSDLRect (relativePos - V2 4.0 4.0) (V2 20.0 20.0)
          SDL.rendererDrawColor renderer $= V4 255 255 255 200
          SDL.fillRect renderer (Just flashRect)
          SDL.rendererDrawColor renderer $= V4 220 100 255 255
          SDL.drawRect renderer (Just flashRect)

      VFXThrust -> case mDir of
        Just dir -> do
          let thrustOffset = if dir == DirLeft then V2 (-36.0) 4.0 else V2 12.0 4.0
              thrustRect = toSDLRect (relativePos + thrustOffset) (V2 48.0 16.0)
          SDL.rendererDrawColor renderer $= V4 255 200 0 180
          SDL.fillRect renderer (Just thrustRect)
          SDL.rendererDrawColor renderer $= V4 255 255 150 240
          SDL.drawRect renderer (Just thrustRect)
        Nothing -> return ()

      VFXShockwave -> do
        let lifeFraction = (0.3 - life) / 0.3
            waveWidth = lifeFraction * 96.0
            waveOffset = V2 (-waveWidth / 2.0) 0.0
            rect = toSDLRect (relativePos + waveOffset) (V2 waveWidth 6.0)
            alpha = round (max 0.0 (min 255.0 ((0.3 - lifeFraction * 0.3) / 0.3 * 200.0)))
        SDL.rendererDrawColor renderer $= V4 255 50 180 alpha
        SDL.fillRect renderer (Just rect)
        SDL.rendererDrawColor renderer $= V4 255 120 220 alpha
        SDL.drawRect renderer (Just rect)
    ) ()

  -- Particles (dust, sparks, sparkles); alpha fades with remaining life.
  ParticleStore particles <- get global
  liftIO $ forM_ particles $ \p -> do
    let (r, g, b) = pColor p
        chan = fromIntegral . max 0 . min (255 :: Int)
        alpha = chan (round (255.0 * max 0.0 (min 1.0 (pLife p / pMaxLife p))))
        sz = pSize p
        rect = toSDLRect (pPos p - camOffset - pure (sz / 2.0)) (pure sz)
    SDL.rendererDrawColor renderer $= V4 (chan r) (chan g) (chan b) alpha
    SDL.fillRect renderer (Just rect)

  -- NPCs: body, name, chatter bubble, and an E hint when the player is close.
  cfoldM_ (\_ (Npc nid, NpcBrain ai, Position pos, Collider size) -> do
    let relativePos = pos - camOffset
        rect = toSDLRect relativePos size
        (r, g, b) = maybe (200, 180, 120) ndColor (M.lookup nid npcDefs)
        chan = fromIntegral . max 0 . min (255 :: Int)
    SDL.rendererDrawColor renderer $= V4 (chan r) (chan g) (chan b) 255
    SDL.fillRect renderer (Just rect)
    SDL.rendererDrawColor renderer $= V4 (chan (r + 50)) (chan (g + 50)) (chan (b + 50)) 255
    SDL.drawRect renderer (Just rect)

    -- Name tag.
    forM_ (M.lookup nid npcDefs) $ \def -> do
      let nameStr = T.unpack (ndName def)
          V2 rx ry = relativePos
          V2 w _ = size
      liftIO $ do
        nameW <- textWidth fonts 1.5 nameStr
        drawText fonts (V4 170 190 215 220) 1.5
          (V2 (rx + (w - nameW) / 2.0) (ry - 12.0)) nameStr

    -- Chatter bubble.
    forM_ (naBubble ai) $ \(msg, _) -> do
      let txt = T.unpack msg
          V2 rx ry = relativePos
          V2 w _ = size
      txtW <- liftIO (textWidth fonts 1.5 txt)
      let bx = rx + (w - txtW) / 2.0
          by = ry - 30.0
      SDL.rendererDrawColor renderer $= V4 12 16 26 220
      SDL.fillRect renderer
        (Just (toSDLRect (V2 (bx - 6.0) (by - 4.0)) (V2 (txtW + 12.0) 16.0)))
      liftIO $ drawText fonts (V4 230 235 245 255) 1.5 (V2 bx by) txt

    -- Interaction hint.
    when (playerPos /= V2 0.0 0.0) $ do
      let d = norm (pos + size / 2.0 - (playerPos + V2 12.0 12.0))
      when (d <= 56.0) $ do
        let V2 rx ry = relativePos
            V2 w _ = size
        liftIO $ drawText fonts (V4 255 230 120 255) 2.0
          (V2 (rx + w / 2.0 - 4.0) (ry - 26.0)) "E"
    ) ()

  -- Enemies: body in their definition colour, hp bar above when damaged.
  cfoldM_ (\_ (Enemy eid, Position pos, Collider size, EnemyHp hp) -> do
    let relativePos = pos - camOffset
        rect = toSDLRect relativePos size
        (r, g, b) = maybe (200, 80, 80) edColor (M.lookup eid enemyDefs)
        chan = fromIntegral . max 0 . min (255 :: Int)
    SDL.rendererDrawColor renderer $= V4 (chan r) (chan g) (chan b) 255
    SDL.fillRect renderer (Just rect)
    SDL.rendererDrawColor renderer $= V4 (chan (r + 60)) (chan (g + 60)) (chan (b + 60)) 255
    SDL.drawRect renderer (Just rect)

    forM_ (M.lookup eid enemyDefs) $ \def ->
      when (hp < edHp def) $ do
        let V2 w _ = size
            frac = max 0.0 (hp / edHp def)
            barPos = relativePos + V2 0.0 (-8.0)
        SDL.rendererDrawColor renderer $= V4 12 16 26 230
        SDL.fillRect renderer (Just (toSDLRect barPos (V2 w 4.0)))
        SDL.rendererDrawColor renderer $= V4 220 60 70 255
        SDL.fillRect renderer (Just (toSDLRect barPos (V2 (w * frac) 4.0)))
    ) ()

  -- Projectiles.
  cfoldM_ (\_ (Projectile _, Position pos, Collider size) -> do
    let rect = toSDLRect (pos - camOffset) size
    SDL.rendererDrawColor renderer $= V4 180 50 255 255
    SDL.fillRect renderer (Just rect)
    SDL.rendererDrawColor renderer $= V4 230 180 255 255
    SDL.drawRect renderer (Just rect)
    ) ()

  -- Grappling hook cable and tip.
  cfoldM_ (\_ (Player, PlayerHook hookState, Position pos, Collider size) -> do
    let mHookTip = case hookState of
          HookFlying hp _ -> Just hp
          HookAnchored hp -> Just hp
          HookRetracted   -> Nothing
    case mHookTip of
      Nothing -> return ()
      Just hookTipPos -> do
        let playerCenter = pos + size / 2.0 - camOffset
            hookTipRel = hookTipPos - camOffset
        SDL.rendererDrawColor renderer $= V4 230 130 30 255
        SDL.drawLine renderer (SDL.P (round <$> playerCenter)) (SDL.P (round <$> hookTipRel))
        let tipRect = toSDLRect (hookTipRel - V2 4.0 4.0) (V2 8.0 8.0)
        SDL.rendererDrawColor renderer $= V4 255 140 0 255
        SDL.fillRect renderer (Just tipRect)
    ) ()

  -- Player.
  cfoldM_ (\_ (Player, Position pos, Collider size, cstate :: CombatState, Facing dir) -> do
    let relativePos = pos - camOffset
        V2 pw ph = size

        -- Plunge squeezes the sprite into a falling spike shape.
        (drawnPos, drawnSize) = case cstate of
          StatePlunge -> (relativePos + V2 ((pw - 16.0) / 2.0) ((ph - 36.0) / 2.0), V2 16.0 36.0)
          _           -> (relativePos, size)

        drawnRect = toSDLRect drawnPos drawnSize
        V2 dpw dph = drawnSize

        (bodyFill, bodyBorder) = case cstate of
          StateDashing _     -> (V4 220 240 255 255, V4 255 255 255 255)
          StateDashJump      -> (V4 160 235 255 255, V4 255 255 255 255)
          StateThrust _      -> (V4 255 200 0 255,   V4 255 255 100 255)
          StatePlunge        -> (V4 200 220 255 255, V4 255 255 255 255)
          StateHookHanging _ -> (V4 0 255 170 255,   V4 100 255 220 255)
          StateCharging t
            | t >= chargeThreshold -> (V4 0 220 220 255, V4 255 220 0 255)
          _                  -> (V4 0 220 220 255,   V4 100 255 255 255)

    SDL.rendererDrawColor renderer $= bodyFill
    SDL.fillRect renderer (Just drawnRect)
    SDL.rendererDrawColor renderer $= bodyBorder
    SDL.drawRect renderer (Just drawnRect)

    -- Fully charged: double border.
    case cstate of
      StateCharging t | t >= chargeThreshold -> do
        let outerRect = toSDLRect (drawnPos - V2 2.0 2.0) (drawnSize + V2 4.0 4.0)
        SDL.drawRect renderer (Just outerRect)
      _ -> return ()

    -- Facing indicator.
    let indX = if dir == DirLeft then 2.0 else dpw - 6.0
        indRect = toSDLRect (drawnPos + V2 indX ((dph - 10.0) / 2.0)) (V2 4.0 10.0)
    SDL.rendererDrawColor renderer $= V4 255 255 255 255
    SDL.fillRect renderer (Just indRect)
    ) ()

  -- The three front parallax layers close the sandwich.
  liftIO $ renderLayerSlots renderer camOffset scene frontSlots

-- | Fill and border colours for an item; the border is a lightened fill.
itemColors :: ItemRegistry -> ItemId -> (V4 Word8, V4 Word8)
itemColors registry iid =
  let (r, g, b) = maybe (200, 200, 200) defColor (lookupItem registry iid)
      chan x = fromIntegral (max 0 (min 255 x))
      lighten x = chan (x + 60)
  in ( V4 (chan r) (chan g) (chan b) 255
     , V4 (lighten r) (lighten g) (lighten b) 255
     )
