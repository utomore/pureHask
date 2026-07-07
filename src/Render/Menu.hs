-- | The in-game menu overlay: tab bar and the eight pages. Reads the world
--   (vitals, positions) and the 'MenuEnv' snapshot; all interaction logic
--   lives in "Flow.Machine" — this module only draws.
module Render.Menu
  ( renderMenu
  ) where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Apecs hiding (($=))
import qualified SDL
import Linear (V2(..), V4(..))
import Text.Printf (printf)

import Core.Components
import Core.Config
import Core.Types
import Items.Registry
import Quest.Script (QuestDef(..), QuestStage(..))
import Render.Draw (toSDLRect, itemColors)
import Render.Font
import Render.Widgets
import World.Tilemap

panelPos :: V2 Double
panelPos = V2 50.0 40.0

panelSize :: V2 Double
panelSize = V2 700.0 520.0

contentX, contentY :: Double
contentX = 74.0
contentY = 110.0

rowH :: Double
rowH = 30.0

textDim, textLit, textHi :: Color
textDim = V4 110 130 160 255
textLit = V4 200 215 235 255
textHi  = V4 255 230 120 255

-- | Draw the whole menu for the current cursor position.
renderMenu :: FontSet -> ItemRegistry -> Tilemap -> MenuCursor -> MenuEnv
           -> RunStats -> Int -> [QuestDef] -> QuestLog -> Game ()
renderMenu fonts registry tilemap cursor env stats levelIx questDefs qlog = do
  vitalsList <- cfold (\acc (Player, v :: Vitals) -> v : acc) []
  playerPosL <- cfold (\acc (Player, Position p) -> p : acc) []
  goalPosL   <- cfold (\acc (Goal, Position p) -> p : acc) []

  liftIO $ do
    -- Dim the scene, draw the panel and the tab bar.
    drawPanel renderer (V2 0 0) (V2 screenWidth screenHeight) (V4 8 10 18 200) (V4 8 10 18 0)
    drawPanel renderer panelPos panelSize (V4 16 22 36 245) (V4 58 74 102 255)

    forM_ (zip [0 :: Int ..] menuPages) $ \(i, page) -> do
      let x = 74.0 + fromIntegral i * 84.0
          color = if page == mcPage cursor then textHi else textDim
      drawText fonts color 2.0 (V2 x 58.0) (menuPageTitle page)
      when (page == mcPage cursor) $ do
        w <- textWidth fonts 2.0 (menuPageTitle page)
        drawPanel renderer (V2 x 74.0) (V2 w 2.0) textHi textHi

    case mcPage cursor of
      PageStatus   -> drawStatus vitalsList
      PageBackpack -> drawBackpackPage
      PageEquip    -> drawEquipPage
      PageQuests   -> drawQuestsPage
      PageMap      -> drawMapPage playerPosL goalPosL
      PageSave     -> drawSlots "SAVE TO:" manualSlotRows
      PageLoad     -> drawSlots "LOAD FROM:" (("AUTO", meAutoSave env) : manualSlotRows)
      PageSettings -> drawSettingsPage

    drawText fonts textDim 1.5 (V2 contentX 530.0)
      "ARROWS: NAVIGATE   SPACE: CONFIRM   ESC: CLOSE"
  where
    renderer = fsRenderer fonts

    centeredIn y color scale str = do
      w <- textWidth fonts scale str
      drawText fonts color scale (V2 ((screenWidth - w) / 2.0) y) str

    row i = V2 contentX (contentY + fromIntegral (i :: Int) * rowH)

    -- The selected row gets a full-width highlight bar plus the ">" marker,
    -- so the cursor is obvious at a glance. Pages call this BEFORE drawing
    -- the row's text, so the bar sits underneath.
    cursorAt i target = when (mcRow cursor == i && mcPage cursor == target) $ do
      let y = contentY + fromIntegral i * rowH
      drawPanel renderer (V2 (contentX - 26.0) (y - 6.0)) (V2 656.0 26.0)
        (V4 52 68 100 160) (V4 255 230 120 220)
      drawText fonts textHi 2.0 (V2 (contentX - 18.0) y) ">"

    drawStatus vitalsList = do
      let v = case vitalsList of
                (x : _) -> x
                []      -> fullVitals playerMaxHp playerMaxMp playerMaxStamina
          line i = drawText fonts textLit 2.0 (row i)
      line 0 (printf "HP      %3.0f / %3.0f" (vHp v) (vMaxHp v))
      line 1 (printf "MP      %3.0f / %3.0f" (vMp v) (vMaxMp v))
      line 2 (printf "STAMINA %3.0f / %3.0f" (vStamina v) (vMaxStamina v))
      line 4 ("LEVEL   " <> show (levelIx + 1))
      line 5 ("TIME    " <> formatTime (statTime stats))
      line 6 ("DEATHS  " <> show (statDeaths stats))
      line 7 ("ITEMS   " <> show (statItems stats))

    drawBackpackPage
      | null (meBackpack env) =
          centeredIn 260.0 textDim 2.5 "BACKPACK IS EMPTY"
      | otherwise = do
          forM_ (zip [0 :: Int ..] (meBackpack env)) $ \(i, (iid, cat, count)) -> do
            cursorAt i PageBackpack
            let V2 x y = row i
                (fill, border) = itemColors registry iid
            drawPanel renderer (V2 x y) (V2 14.0 14.0)
              (fromIntegral <$> fill) (fromIntegral <$> border)
            drawText fonts textLit 2.0 (V2 (x + 26.0) y)
              (T.unpack (itemDisplayName registry iid))
            drawText fonts textDim 2.0 (V2 (x + 300.0) y) ("X" <> show count)
            drawText fonts textDim 2.0 (V2 (x + 380.0) y) (categoryTag cat)
          -- Description of the selected item.
          case drop (mcRow cursor) (meBackpack env) of
            ((iid, _, _) : _) ->
              case lookupItem registry iid of
                Just def -> drawText fonts textDim 1.5 (V2 contentX 490.0)
                              (T.unpack (defDesc def))
                Nothing  -> return ()
            [] -> return ()

    drawEquipPage =
      forM_ (zip [0 :: Int ..] (meEquipped env)) $ \(i, (slot, mItem)) -> do
        cursorAt i PageEquip
        let V2 x y = row i
        drawText fonts textDim 2.0 (V2 x y) (slotName slot)
        case mItem of
          Just iid -> drawText fonts textLit 2.0 (V2 (x + 140.0) y)
                        (T.unpack (itemDisplayName registry iid))
          Nothing  -> drawText fonts (V4 70 85 110 255) 2.0 (V2 (x + 140.0) y) "-"

    drawQuestsPage
      | null questDefs = centeredIn 260.0 textDim 2.5 "NO QUESTS"
      | otherwise =
          forM_ (zip [0 :: Int ..] questDefs) $ \(i, qd) -> do
            let V2 x y = row (i * 2)
                prog = M.findWithDefault (QuestProgress QAvailable 0 0)
                         (qdId qd) (qlQuests qlog)
                (tag, tagColor) = case qpPhase prog of
                  QAvailable -> ("NEW",  textDim)
                  QActive    -> ("OPEN", textHi)
                  QDone      -> ("DONE", V4 120 255 190 255)
                goal = case drop (qpStage prog) (qdStages qd) of
                  (st : _) | qpPhase prog == QActive -> T.unpack (qsGoal st)
                  _ -> ""
            drawText fonts textLit 2.0 (V2 x y) (T.unpack (qdName qd))
            drawText fonts tagColor 2.0 (V2 (x + 340.0) y) tag
            when (goal /= "") $
              drawText fonts textDim 1.5 (V2 (x + 12.0) (y + 16.0)) goal

    drawMapPage = \pps gps -> do
      let availW = 640.0
          availH = 360.0
          sc = min (availW / mapPixelWidth tilemap) (availH / mapPixelHeight tilemap)
          origin = V2 contentX (contentY + 10.0)
          toMini p = origin + p * pure sc
      drawPanel renderer (origin - V2 4.0 4.0)
        (V2 (mapPixelWidth tilemap * sc + 8.0) (mapPixelHeight tilemap * sc + 8.0))
        (V4 10 14 24 255) (V4 58 74 102 255)
      forM_ [0 .. mapHeight tilemap - 1] $ \r ->
        forM_ [0 .. mapWidth tilemap - 1] $ \c ->
          when (isTileSolid tilemap c r) $ do
            let p = toMini (V2 (fromIntegral c * tileSize) (fromIntegral r * tileSize))
            SDL.rendererDrawColor renderer SDL.$= V4 43 55 77 255
            SDL.fillRect renderer (Just (toSDLRect p (pure (max 1.0 (tileSize * sc)))))
      forM_ gps $ \g ->
        drawPanel renderer (toMini g) (V2 5.0 5.0) (V4 220 50 180 255) (V4 255 100 220 255)
      forM_ pps $ \p ->
        drawPanel renderer (toMini p) (V2 5.0 5.0) (V4 0 220 220 255) (V4 150 255 255 255)

    manualSlotRows =
      [ ("SLOT " <> show (i + 1), s)
      | (i, s) <- zip [(0 :: Int) ..] (meSaveSlots env)
      ]

    drawSlots title slotList = do
      drawText fonts textDim 2.0 (V2 contentX (contentY - 34.0)) title
      forM_ (zip [0 :: Int ..] slotList) $ \(i, (label, mSummary)) -> do
        cursorAt i (mcPage cursor)
        let V2 x y = row i
        drawText fonts textLit 2.0 (V2 x y)
          (label <> "  " <> maybe "EMPTY" id mSummary)

    drawSettingsPage =
      forM_ (zip [0 :: Int ..] (meSettings env)) $ \(i, (label, value)) -> do
        cursorAt i PageSettings
        let V2 x y = row i
        drawText fonts textLit 2.0 (V2 x y) label
        drawText fonts (if value == "OFF" then textDim else textHi) 2.0
          (V2 (x + 260.0) y) value

    slotName slot = case slot of
      SlotWeapon -> "WEAPON"
      SlotBody   -> "BODY"
      SlotShoes  -> "SHOES"
      SlotGloves -> "GLOVES"
      SlotHead   -> "HEAD"

    categoryTag cat = case cat of
      CatGeneral -> "GEN"
      CatPotion  -> "POT"
      CatQuest   -> "QST"
      CatEquip _ -> "EQP"

formatTime :: Double -> String
formatTime t =
  let total = floor t :: Int
      (m, s) = total `divMod` 60
  in printf "%02d:%02d" m s
