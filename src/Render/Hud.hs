-- | The always-on in-game HUD: vitals bars (HP red, MP blue, stamina
--   yellow), the quest tracker line and toast messages. Reads the ECS world;
--   never writes.
module Render.Hud
  ( renderHud
  ) where

import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as T
import Apecs
import qualified SDL
import Linear (V2(..), V4(..))

import Core.Components
import Core.Config (screenWidth, screenHeight)
import Core.Types
import Render.Font (drawText, textWidth)
import Render.Widgets

-- | Draw the HUD: vitals top-left, quest tracker top-right, toasts
--   bottom-center.
renderHud :: SDL.Renderer -> [Text] -> Maybe (Text, Text) -> Game ()
renderHud renderer toasts tracker = do
  vitalsList <- cfold (\acc (Player, v :: Vitals) -> v : acc) []
  liftIO $ do
    case vitalsList of
      [] -> return ()
      (v : _) -> do
        let barSize = V2 140.0 10.0
            row i = V2 12.0 (12.0 + i * 16.0)
        drawLabelledBar renderer (row 0) barSize (vHp v / vMaxHp v)
          (V4 220 60 70 255) "HP"
        drawLabelledBar renderer (row 1) barSize (vMp v / vMaxMp v)
          (V4 70 120 230 255) "MP"
        drawLabelledBar renderer (row 2) barSize (vStamina v / vMaxStamina v)
          (V4 230 200 60 255) "ST"

    -- Quest tracker under the level label (top-right).
    forM_ tracker $ \(_questName, goal) -> do
      let txt = T.unpack goal
      drawText renderer (V4 230 200 60 220) 1.5
        (V2 (screenWidth - textWidth 1.5 txt - 12.0) 34.0) txt

    -- Toasts, oldest on top, stacked above the bottom edge.
    forM_ (zip [0 :: Int ..] toasts) $ \(i, msg) -> do
      let txt = T.unpack msg
          y = screenHeight - 90.0 + fromIntegral i * 22.0
          x = (screenWidth - textWidth 2.0 txt) / 2.0
      drawPanel renderer (V2 (x - 8.0) (y - 4.0))
        (V2 (textWidth 2.0 txt + 16.0) 20.0) (V4 12 16 26 200) (V4 58 74 102 255)
      drawText renderer (V4 220 230 245 255) 2.0 (V2 x y) txt
