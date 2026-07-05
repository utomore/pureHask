-- | UI overlays: mode screens (title, pause, death, level complete, ending)
--   and the in-game backpack overlay. Drawn on top of the scene each frame.
module Render.UI
  ( renderOverlay
  , renderBackpack
  ) where

import Control.Monad (when, forM_)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Apecs hiding (($=))
import SDL (($=))
import qualified SDL
import Linear (V2(..), V4(..))
import Text.Printf (printf)

import Core.Components
import Core.Config
import Core.Types
import Items.Registry (ItemRegistry, itemDisplayName)
import Render.Draw (toSDLRect, itemColors)
import Render.Font

-- | Dim the whole screen with the given colour/alpha.
dimScreen :: SDL.Renderer -> V4 Int -> IO ()
dimScreen renderer (V4 r g b a) = do
  SDL.rendererDrawColor renderer $=
    V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
  SDL.fillRect renderer (Just (toSDLRect (V2 0 0) (V2 screenWidth screenHeight)))

-- | Horizontally centered text.
centered :: SDL.Renderer -> V4 Int -> Double -> Double -> String -> IO ()
centered renderer color scale y str =
  drawText renderer color scale (V2 ((screenWidth - textWidth scale str) / 2.0) y) str

-- | Draw the overlay matching the current mode.
renderOverlay :: SDL.Renderer -> GameMode -> RunStats -> Int -> IO ()
renderOverlay renderer mode stats levelIx = case mode of
  ModeTitle -> do
    dimScreen renderer (V4 10 14 24 235)
    centered renderer (V4 0 220 220 255) 10.0 160.0 "PUREHASK"
    centered renderer (V4 160 190 220 255) 3.0 300.0 "PRESS SPACE TO START"
    centered renderer (V4 90 110 140 255) 2.5 360.0 "ESC TO QUIT"

  ModePlaying -> do
    let label = "LEVEL " <> show (levelIx + 1)
    drawText renderer (V4 120 150 190 200) 2.0
      (V2 (screenWidth - textWidth 2.0 label - 12.0) 12.0) label

  -- The menu is drawn by "Render.Menu"; nothing extra here.
  ModeMenu -> return ()

  -- Dialogue box at the bottom of the screen, one line at a time.
  ModeDialogue pending -> case pending of
    [] -> return ()
    ((speaker, line) : _) -> do
      let boxPos = V2 60.0 (screenHeight - 130.0)
          boxSize = V2 (screenWidth - 120.0) 96.0
      SDL.rendererDrawColor renderer $= V4 14 19 32 235
      SDL.fillRect renderer (Just (toSDLRect boxPos boxSize))
      SDL.rendererDrawColor renderer $= V4 58 74 102 255
      SDL.drawRect renderer (Just (toSDLRect boxPos boxSize))
      drawText renderer (V4 255 230 120 255) 2.0 (boxPos + V2 16.0 12.0)
        (T.unpack speaker)
      drawText renderer (V4 220 230 245 255) 2.5 (boxPos + V2 16.0 40.0)
        (T.unpack line)
      drawText renderer (V4 110 130 160 255) 1.5 (boxPos + V2 16.0 74.0)
        "SPACE: NEXT"

  ModeDead _ -> do
    dimScreen renderer (V4 60 10 20 150)
    centered renderer (V4 255 90 110 255) 6.0 260.0 "YOU DIED"

  ModeLevelComplete _ -> do
    dimScreen renderer (V4 10 30 24 150)
    centered renderer (V4 120 255 190 255) 5.0 260.0 "LEVEL COMPLETE"

  ModeEnding -> do
    dimScreen renderer (V4 10 14 24 235)
    centered renderer (V4 255 220 120 255) 8.0 140.0 "THE END"
    centered renderer (V4 200 215 235 255) 3.0 280.0 ("TIME " <> formatTime (statTime stats))
    centered renderer (V4 200 215 235 255) 3.0 320.0 ("DEATHS " <> show (statDeaths stats))
    centered renderer (V4 200 215 235 255) 3.0 360.0 ("ITEMS " <> show (statItems stats))
    centered renderer (V4 90 110 140 255) 2.5 430.0 "SPACE TO TITLE"

formatTime :: Double -> String
formatTime t =
  let total = floor t :: Int
      (m, s) = total `divMod` 60
  in printf "%02d:%02d" m s

-- | The backpack overlay (toggled with I while playing).
renderBackpack :: SDL.Renderer -> ItemRegistry -> Game ()
renderBackpack renderer registry = do
  UIState shown <- get global
  when shown $ do
    backpacks <- cfold (\acc (Player, Backpack b) -> b : acc) []
    let items = case backpacks of
          (b : _) -> M.toAscList b
          []      -> []

    liftIO $ do
      let uiRect = toSDLRect (V2 580.0 40.0) (V2 180.0 320.0)
      SDL.rendererDrawColor renderer $= V4 25 35 50 220
      SDL.fillRect renderer (Just uiRect)
      SDL.rendererDrawColor renderer $= V4 58 74 102 255
      SDL.drawRect renderer (Just uiRect)
      drawText renderer (V4 160 190 220 255) 2.0 (V2 600.0 48.0) "BACKPACK"

      forM_ (zip [(0 :: Int) .. 4] (map Just items ++ repeat Nothing)) $ \(idx, mItem) -> do
        let slotY = 72.0 + fromIntegral idx * 56.0
            slotRect = toSDLRect (V2 600.0 slotY) (V2 40.0 40.0)
        SDL.rendererDrawColor renderer $= V4 15 20 30 255
        SDL.fillRect renderer (Just slotRect)
        SDL.rendererDrawColor renderer $= V4 43 55 77 255
        SDL.drawRect renderer (Just slotRect)

        case mItem of
          Just (item, count) -> do
            let iconRect = toSDLRect (V2 612.0 (slotY + 12.0)) (V2 16.0 16.0)
                (fill, border) = itemColors registry item
            SDL.rendererDrawColor renderer $= fill
            SDL.fillRect renderer (Just iconRect)
            SDL.rendererDrawColor renderer $= border
            SDL.drawRect renderer (Just iconRect)
            when (count > 1) $
              drawText renderer (V4 255 255 255 255) 1.5 (V2 604.0 (slotY + 30.0))
                ("X" <> show count)
            drawText renderer (V4 150 170 200 255) 1.5 (V2 646.0 (slotY + 16.0))
              (T.unpack (itemDisplayName registry item))
          Nothing -> return ()
