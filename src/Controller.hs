-- | This module defines how the state changes
--   in response to time and user input

{-# language NamedFieldPuns #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.IO
import Control.Monad
import Data.List
import Data.Maybe

-- Function that handles 1 iteration of the game.
step :: Float -> GameState -> IO GameState
step secs gstate@GameState {pacman = Player {playerPosition = (x,y), playerDirection, playerSpeed = speed, playerLives = lives}, maze = maze}
      | speed == Stopped                                              = return gstate        
      | fieldIn16 gstate == MazeField{field = Wall, content = Empty}  = return gstate
      | hasFoodDot (getMazeField(playerLocationInMaze(x,y)) maze)     = return (eatFoodDotsGameState(playerLocationInMaze(x,y)) gstate)
      | numberOfFoodDots maze == 0                                    = return $ gameWonGameState gstate      
      | lives == 0                                                    = return $ gameLostGameState gstate     
      | otherwise                                                     = makeStep gstate

-- This functions handles the removal of FoodDots of the maze in the gamestate. It also updates the score.      
eatFoodDotsGameState :: (Int,Int) -> GameState -> GameState
eatFoodDotsGameState (a,b) gstate@GameState{maze = oldMaze, score} = gstate{maze = newMaze, score = score + 10}
 where newMaze = eatFoodDot (a,b) oldMaze
    
-- This function changes status of de game to GameLost    
gameLostGameState :: GameState -> GameState
gameLostGameState gstate = gstate {status = GameLost}

-- This function changes status of de game to GameWon   
gameWonGameState :: GameState -> GameState
gameWonGameState gstate = gstate {status = GameWon}

-- This function changes the current speed of Pacman. Used to pause the game
pauseGame :: GameState -> GameState
pauseGame gstate@GameState {pacman = Player{playerSpeed = speed, playerPosition, playerDirection, playerLives}}
  | speed == Normal    = gstate {pacman = Player{playerSpeed = Stopped, playerPosition, playerDirection, playerLives}}
  | otherwise          = gstate {pacman = Player{playerSpeed = Normal, playerPosition, playerDirection, playerLives}}

-- Function that handles user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- If the user presses an arrow, update Pac-man's Location
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate    =  movePacmanUp 0 gstate
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate  =  movePacmanDown 0 gstate
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate  =  movePacmanLeft 0 gstate
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate =  movePacmanRight 0 gstate
inputKey (EventKey (SpecialKey KeyF1) Down _ _) gstate =  pauseGame gstate  -- If the user presses F1, pause game
inputKey _ gstate = gstate -- Otherwise keep the same

-- This functions calls a movePacman function, based on the current direction
makeStep :: GameState -> IO GameState
makeStep gstate@GameState {pacman = Player {playerDirection = dir}}
      | dir == FaceUp     = return (movePacmanUp 1 gstate)
      | dir == FaceDown   = return (movePacmanDown 1 gstate)
      | dir == FaceLeft   = return (movePacmanLeft 1 gstate)
      | dir == FaceRight  = return (movePacmanRight 1 gstate)             

-- This function changes the location and direction of pacman.
movePacmanUp :: Float -> GameState -> GameState
movePacmanUp dy gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed, playerLives}} = 
  gstate{pacman = Player{playerPosition = (x, y + dy), playerDirection = FaceUp, playerSpeed, playerLives}}

-- This function changes the location and direction of pacman.
movePacmanDown :: Float -> GameState -> GameState
movePacmanDown dy gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed, playerLives}} = 
  gstate{pacman = Player{playerPosition = (x, y - dy), playerDirection = FaceDown, playerSpeed, playerLives}}

-- This function changes the location and direction of pacman.  
movePacmanLeft :: Float -> GameState -> GameState
movePacmanLeft dx gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed, playerLives}} = 
  gstate{pacman = Player{playerPosition = (x - dx, y), playerDirection = FaceLeft, playerSpeed, playerLives}}

-- This function changes the location and direction of pacman.  
movePacmanRight :: Float -> GameState -> GameState
movePacmanRight dx gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed, playerLives}} = 
  gstate{pacman = Player{playerPosition = (x + dx, y), playerDirection = FaceRight, playerSpeed, playerLives}}
      
-- Function that calculates the distance between two provided points.
calculateDistance :: Point -> Point -> Float
calculateDistance (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

-- Function that updates the score provided by the GameState.
changeScore :: Int -> GameState -> GameState
changeScore points GameState{score} = GameState{score = score + points}

-- Function that decreases the amount of lives Pacman has in the GameState.
-- Whenever he has no lives left the status of the game is changed to GameOver.
decreaseLives :: GameState -> GameState
decreaseLives GameState{score = currentScore, status = gamestatus, pacman = Player{playerLives}} = 
  GameState{pacman = Player{playerLives = playerLives - 1}}

-- Right now if 2 tiles are the same distance from the target tile, the first tile
-- of these 2 is chosen, but it should be different. If two tiles have the same distance 
-- to the target tile the order should be up > left > down > right.
determinePath :: Point -> [Point] -> Point
determinePath targetTile possiblePaths = possiblePaths !! indexOfShortestDistance
  where distances = map (calculateDistance targetTile) possiblePaths 
        shortestDistance = minimum distances
        indexOfShortestDistance = fromMaybe 0 (elemIndex shortestDistance distances)

-- Function that calculates Blinky's target tile when he is in Chase mode using the GameState.
-- Blinky's target tile is the same as Pacman's position. 
blinkyChaseTarget :: GameState -> Point
blinkyChaseTarget GameState{pacman = Player{playerPosition = (x, y)}} = (x, y)

-- Constant that defines Blinky's target tile when he is in Scatter mode.
blinkyScatterTarget :: Point
blinkyScatterTarget = (26,1)

-- Function that calculates Pinky's target tile when he is in Chase mode using the GameState.
-- Pinky's target tile is four tiles in front of wherever Pacman is facing.
pinkyChaseTarget :: GameState -> Point
pinkyChaseTarget GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} 
  | dir == FaceUp   = (x, y - 4)
  | dir == FaceDown = (x, y + 4)
  | dir == FaceLeft = (x - 4, y)
  | otherwise       = (x + 4, y)

-- Constant that defines Pinky's target tile when he is in Scatter mode. 
pinkyScatterTarget :: Point
pinkyScatterTarget = (3,1)

-- Function that calculates Inky's target tile when he is in Chase mode using the GameState.
-- Inky's target tile is Blinky's position mirrored in the tile that is two tiles in front of
-- wherever Pacman is facing.
inkyChaseTarget :: GameState -> Point
inkyChaseTarget GameState{pacman = Player{playerPosition = (x0,y0), playerDirection = dir}, blinky = Ghost{ghostPosition = (x1, y1)}} = mirrorPoint + mirrorDelta
  where mirrorPoint | dir == FaceUp   = (x0, y0 - 2)
                    | dir == FaceDown = (x0, y0 + 2)
                    | dir == FaceLeft = (x0 - 2, y0)
                    | otherwise       = (x0 + 2, y0)
        mirrorDelta = mirrorPoint - (x1, y1)

-- Constant that defines Inky's target that he tries to reach when he is in Scatter mode.        
inkyScatterTarget :: Point
inkyScatterTarget = (28,36)

-- Funtion that calculates Clyde's target tile when he is in Chase mode using the GameState.
-- Clyde's target tile is the same as Blinky's target tile whenever Clyde is further than 8 tiles
-- removed from Pacman and his Scatter target whenever he is less than 8 tiles away.
clydeChaseTarget :: GameState -> Point
clydeChaseTarget GameState{pacman = Player{playerPosition = (x0,y0), playerDirection = dir}, blinky = Ghost{ghostPosition = (x1, y1)}} 
  | distancePacmanClyde > 8 = blinkyChaseTarget GameState{pacman = Player{playerPosition = (x0, y0)}}
  | otherwise = clydeScatterTarget
    where distancePacmanClyde = calculateDistance (x0, y0) (x1, y1)

-- Constant that defines Clyde's target tile when he is in Scatter mode.
clydeScatterTarget :: Point
clydeScatterTarget = (1,36)

-- Funtion that gets the highscore from the specified file.
getHighScore :: String -> IO String
getHighScore = readFileStrict

-- Function that gets the score at the end of the game as a parameter and uses this to
-- update the highscore file, but only when to supplied score is higher than the score
-- specified in the file.
updateHighScore :: String -> String -> IO ()
updateHighScore score file = do s <- getHighScore file
                                when (s < score) $ writeFile file score

-- Below methodes taken from https://github.com/nh2/shake/blob/e0c4bda9943bfadc9383ec31cfe828d67879e8ca/Development/Shake/Derived.hs to circumvent the "open file resource busy (file is locked)" error
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s

readFileStrict :: FilePath -> IO String
readFileStrict name =  openFile name ReadMode >>= hGetContentsStrict