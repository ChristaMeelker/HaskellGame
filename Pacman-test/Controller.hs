-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate  = return gstate

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- If the user presses an arrow, update Pac-man's location
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate =  movePacmanUp 5 gstate
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate =  movePacmanLeft 5 gstate
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate =  movePacmanRight 5 gstate
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate =  movePacmanDown 5 gstate
-- Otherwise keep the same
inputKey _ gstate = gstate

movePacmanUp :: Float -> GameState -> GameState
movePacmanUp dy GameState{pacmanLoc = (x,y)} = GameState{pacmanLoc = (x, y + dy)}

movePacmanLeft :: Float -> GameState -> GameState
movePacmanLeft dx GameState{pacmanLoc = (x,y)} = GameState{pacmanLoc = (x - dx, y)}

movePacmanRight :: Float -> GameState -> GameState
movePacmanRight dx GameState{pacmanLoc = (x,y)} = GameState{pacmanLoc = (x + dx, y)}

movePacmanDown :: Float -> GameState -> GameState
movePacmanDown dy GameState{pacmanLoc = (x,y)} = GameState{pacmanLoc = (x, y - dy)}