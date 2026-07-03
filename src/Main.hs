module Main where

import Control.Monad (unless)
import Data.Word (Word32)
import Apecs
import Linear (V2(..))
import qualified SDL

import Types
import Map
import Physics
import Render
import FRP

-- | Processes SDL2 events and returns whether the player wants to quit,
--   along with the updated keyboard InputState.
processEvents :: [SDL.Event] -> InputState -> (Bool, InputState)
processEvents events input = foldl process (False, input) events
  where
    process (q, inp) event =
      case SDL.eventPayload event of
        SDL.QuitEvent -> (True, inp)
        SDL.KeyboardEvent keyboardEvent ->
          let
            isDown = SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
            keyCode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
          in case keyCode of
            SDL.KeycodeEscape -> (True, inp)
            SDL.KeycodeLeft   -> (q, inp { inputLeft  = isDown })
            SDL.KeycodeRight  -> (q, inp { inputRight = isDown })
            SDL.KeycodeA      -> (q, inp { inputLeft  = isDown })
            SDL.KeycodeD      -> (q, inp { inputRight = isDown })
            SDL.KeycodeSpace  -> (q, inp { inputJump  = isDown })
            _                 -> (q, inp)
        _ -> (q, inp)

-- | Main entry point of the game application.
main :: IO ()
main = do
  -- 1. Initialize SDL2
  SDL.initializeAll
  
  -- Create window
  window <- SDL.createWindow "pureHask 2D Platformer (Reflex + Apecs)" SDL.defaultWindow
    { SDL.windowInitialSize = V2 800 600 }
    
  -- Create renderer with hardware acceleration and VSync enabled
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
    { SDL.rendererType          = SDL.AcceleratedVSyncRenderer
    , SDL.rendererTargetTexture = False
    }

  -- 2. Parse the level design
  let (tilemap, playerSpawn, goalPos) = parseTilemap defaultMapLayout
      mapHeightLimit = fromIntegral (mapHeight tilemap) * tileSize
      
  -- 3. Initialize the Apecs World
  world <- initWorld
  
  -- Spawn entities inside ECS
  runSystem (do
    -- Spawn Player (Cyan Square, size 24x24)
    _ <- newEntity ( Player
                   , Position playerSpawn
                   , Velocity (V2 0.0 0.0)
                   , Collider (V2 24.0 24.0)
                   , Gravity gravityAccel
                   , IsGrounded False
                   )
    -- Spawn Goal (Magenta Square, size 32x32)
    _ <- newEntity ( Goal
                   , Position goalPos
                   , Collider (V2 32.0 32.0)
                   )
    return ()
    ) world

  -- 4. Setup the Reflex event network
  (triggers, _) <- setupReflex

  -- 5. Start the main game loop
  startTime <- SDL.ticks
  gameLoop startTime triggers emptyInputState window renderer world tilemap playerSpawn goalPos mapHeightLimit

  -- 6. Cleanup SDL2 resources upon exiting
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

-- | The core game loop that pumps SDL2 events and ticks the Reflex network.
gameLoop :: Word32
         -> GameTriggers
         -> InputState
         -> SDL.Window
         -> SDL.Renderer
         -> World
         -> Tilemap
         -> V2 Double
         -> V2 Double
         -> Double
         -> IO ()
gameLoop lastTime triggers inputState window renderer world tilemap playerSpawn goalPos mapHeightLimit = do
  currentTime <- SDL.ticks
  
  -- Calculate frame delta-time (in seconds)
  let dt = fromIntegral (currentTime - lastTime) / 1000.0
      -- Cap delta-time to avoid huge physics jumps if the window is moved or lags
      dt' = min 0.1 dt
      
  -- Poll all pending SDL2 window & input events
  events <- SDL.pollEvents
  let (quit, inputState') = processEvents events inputState
  
  unless quit $ do
    -- Push the new input state to Reflex
    fireInput triggers inputState'
    -- Tick the physics simulation and rendering network, obtaining combined values
    mOccur <- fireTick triggers dt'
    
    case mOccur of
      Just (input, simDt) -> do
        -- Run the Apecs update and render the frame
        runSystem (do
          controlPlayer input
          stepPhysics tilemap simDt
          checkWinLoss playerSpawn goalPos mapHeightLimit
          renderGame renderer tilemap
          ) world
      Nothing -> return ()
      
    -- Recursively loop
    gameLoop currentTime triggers inputState' window renderer world tilemap playerSpawn goalPos mapHeightLimit
