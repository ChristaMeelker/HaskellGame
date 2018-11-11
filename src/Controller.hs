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
import Data.Maybe
import Data.List

-- Function that handles 1 iteration of the game.
step :: Float -> GameState -> IO GameState
step secs gstate@GameState {score = currentScore, pacman = Player {playerPosition = (x,y), playerDirection, playerSpeed = speed, playerLives = lives}, maze = maze} 
      | numberOfFoodDots maze == 0                                    = gameWonGameState (show currentScore) "highscore.txt" gstate      
      | lives == 0                                                    = gameLostGameState (show currentScore) "highscore.txt" gstate
      | otherwise                                                     = makeStep gstate

-- This functions handles the removal of FoodDots of the maze in the gamestate. It also updates the score.      
eatFoodDotsGameState :: Point -> GameState -> GameState
eatFoodDotsGameState (a,b) gstate@GameState{maze = oldMaze, score} = gstate{maze = newMaze, score = score + 10}
 where newMaze = eatFoodDot (a,b) oldMaze
    
-- This function changes status of de game to GameLost    
gameLostGameState :: String -> String -> GameState -> IO GameState
gameLostGameState score file gstate = do updateHighScore score file
                                         return gstate {status = GameLost}

-- This function changes status of de game to GameWon   
gameWonGameState :: String -> String -> GameState -> IO GameState
gameWonGameState score file gstate = do updateHighScore score file
                                        return gstate {status = GameWon}

-- Function that changes Pacman's speed to 0 and is used when the game gets paused.
changePacmanSpeedToZero :: GameState -> GameState
changePacmanSpeedToZero gstate@GameState {pacman = Player{playerSpeed = speed, playerPosition, playerDirection, playerLives}} = gstate {pacman = Player{playerSpeed = 0, playerPosition, playerDirection, playerLives}}

-- This changes status and gets everyone's speed to 0.
pauseGame :: GameState -> GameState
pauseGame gstate@GameState {pacman = Player{playerSpeed = speed, playerPosition, playerDirection, playerLives}, blinky = Ghost{ghostDirection, ghostPosition, ghostSpeed = bspeed}, status = status}
  | status == GamePaused       = gstate {pacman = Player{playerSpeed = 3, playerPosition, playerDirection, playerLives}, blinky = Ghost{ghostDirection, ghostPosition, ghostSpeed = 3}, status = GameOn}
  | otherwise                  = gstate {pacman = Player{playerSpeed = 0, playerPosition, playerDirection, playerLives}, blinky = Ghost{ghostDirection, ghostPosition, ghostSpeed = 0}, status = GamePaused}

-- Function that handles user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- If the user presses an arrow, update Pac-man's Location
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate    =  movePacmanUp 0 gstate
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate  =  movePacmanDown 0 gstate
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate  =  movePacmanLeft 0 gstate
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate =  movePacmanRight 0 gstate
inputKey (EventKey (SpecialKey KeyF1) Down _ _) gstate    =  pauseGame gstate  -- If the user presses F1, pause game
inputKey _ gstate = gstate -- Otherwise keep the same

-- This functions calls a movePacman function, based on the current direction
makeStep :: GameState -> IO GameState
makeStep gstate@GameState {pacman = Player {playerDirection = dir, playerSpeed = speed, playerPosition = (x,y)},blinky = Ghost{ghostDirection = bdir, ghostPosition = (a,b)}, maze = maze, status = status}
      | status == GamePaused                                                 = return gstate
      | fieldIn16Ghost gstate == MazeField{field = Wall, content = Empty}    = return $ changeGhostdirection gstate
      | playerLocationInMaze(x,y)  == playerLocationInMaze(a,b)              = return $ decreaseLives gstate
      | hasFoodDot (getMazeField(playerLocationInMaze(x,y)) maze)            = return (eatFoodDotsGameState(playerLocationInMaze(x,y)) gstate)
      | dir == FaceUp     = return (movePacmanUp speed gstate)
      | dir == FaceDown   = return (movePacmanDown speed gstate)
      | dir == FaceLeft   = return (movePacmanLeft speed gstate)
      | dir == FaceRight  = return (movePacmanRight speed gstate)             

-- This method changes the direction of the Ghost, based on the determineDirection function
changeGhostdirection :: GameState -> GameState
changeGhostdirection gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed, playerLives}, blinky = Ghost{ghostDirection = dir, ghostPosition}} =
  gstate{blinky = Ghost{ghostDirection = newDirection, ghostPosition}}
    where newDirection = determineDirection ((x + 16) / 32, 31 - ((y - 16) / 32)) gstate

-- coordinatesX :: Float -> Float -> Float -> Point  
-- These are used to change pacman's location in each direction
coordinatesPacmanUp :: Float -> Float -> Float -> Point
coordinatesPacmanUp x y dy = (x, y + dy)

coordinatesPacmanDown :: Float -> Float -> Float -> Point
coordinatesPacmanDown x y dy = (x, y - dy)

coordinatesPacmanLeft :: Float -> Float -> Float -> Point
coordinatesPacmanLeft x y dy = (x - dy, y)

coordinatesPacmanRight :: Float -> Float -> Float -> Point
coordinatesPacmanRight x y dy = (x + dy, y)

-- This function moves only the Ghost       
moveGhostOnly :: Direction -> GameState -> GameState       
moveGhostOnly dir gstate@GameState{pacman = Player{playerPosition, playerDirection, playerSpeed, playerLives}, blinky = Ghost{ghostDirection = bdir, ghostPosition = (a,b)}} 
  | bdir == FaceUp      = gstate{pacman = Player{playerPosition, playerDirection = dir, playerSpeed = 3, playerLives}, blinky = Ghost{ghostDirection = FaceUp, ghostPosition = (a,b + 3)}}
  | bdir == FaceDown    = gstate{pacman = Player{playerPosition, playerDirection = dir, playerSpeed = 3, playerLives}, blinky = Ghost{ghostDirection = FaceDown, ghostPosition = (a,b - 3)}}
  | bdir == FaceLeft    = gstate{pacman = Player{playerPosition, playerDirection = dir, playerSpeed = 3, playerLives}, blinky = Ghost{ghostDirection = FaceLeft, ghostPosition = (a - 3,b)}}
  | bdir == FaceRight   = gstate{pacman = Player{playerPosition, playerDirection = dir, playerSpeed = 3, playerLives}, blinky = Ghost{ghostDirection = FaceRight, ghostPosition = (a + 3,b)}}

-- This functions moves both pacman and Ghost in the right direction
movePacmanAndGhost :: Direction -> Point -> GameState -> GameState
movePacmanAndGhost dir changedCoordinates gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed = speed, playerLives}, blinky = Ghost{ghostDirection = bdir, ghostPosition = (a,b)}}
  | bdir == FaceUp      = gstate{pacman = Player{playerPosition = changedCoordinates, playerDirection = dir, playerSpeed = 3, playerLives}, blinky = Ghost{ghostDirection = FaceUp, ghostPosition = (a,b + 3)}}
  | bdir == FaceDown    = gstate{pacman = Player{playerPosition = changedCoordinates, playerDirection = dir, playerSpeed = 3, playerLives}, blinky = Ghost{ghostDirection = FaceDown, ghostPosition = (a,b - 3)}}
  | bdir == FaceLeft    = gstate{pacman = Player{playerPosition = changedCoordinates, playerDirection = dir, playerSpeed = 3, playerLives}, blinky = Ghost{ghostDirection = FaceLeft, ghostPosition = (a - 3,b)}}
  | bdir == FaceRight   = gstate{pacman = Player{playerPosition = changedCoordinates, playerDirection = dir, playerSpeed = 3, playerLives}, blinky = Ghost{ghostDirection = FaceRight, ghostPosition = (a + 3,b)}}

-- These functions changes the location and direction of pacman.
movePacmanUp :: Float -> GameState -> GameState
movePacmanUp dy gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed = speed, playerLives}, blinky = Ghost{ghostDirection = bdir, ghostPosition = (a,b)}}
  | fieldIn16 gstate == MazeField{field = Wall, content = Empty} = moveGhostOnly FaceUp gstate
  | otherwise     = movePacmanAndGhost FaceUp (coordinatesPacmanUp x y speed) gstate

movePacmanDown :: Float -> GameState -> GameState
movePacmanDown dy gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed = speed, playerLives}, blinky = Ghost{ghostDirection = bdir, ghostPosition = (a,b)}}
  | fieldIn16 gstate == MazeField{field = Wall, content = Empty} = moveGhostOnly FaceDown gstate
  | otherwise     = movePacmanAndGhost FaceDown (coordinatesPacmanDown x y speed) gstate

movePacmanLeft :: Float -> GameState -> GameState
movePacmanLeft dy gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed = speed, playerLives}, blinky = Ghost{ghostDirection = bdir, ghostPosition = (a,b)}}
  | fieldIn16 gstate == MazeField{field = Wall, content = Empty} = moveGhostOnly FaceLeft gstate
  | otherwise     = movePacmanAndGhost FaceLeft (coordinatesPacmanLeft x y speed) gstate

movePacmanRight :: Float -> GameState -> GameState
movePacmanRight dy gstate@GameState{pacman = Player{playerPosition = (x,y), playerDirection, playerSpeed = speed, playerLives}, blinky = Ghost{ghostDirection = bdir, ghostPosition = (a,b)}}
  | fieldIn16 gstate == MazeField{field = Wall, content = Empty} = moveGhostOnly FaceRight gstate
  | otherwise     = movePacmanAndGhost FaceRight (coordinatesPacmanRight x y speed) gstate
 
-- Function that calculates the distance between two provided points.
calculateDistance :: Point -> Point -> Float
calculateDistance (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

-- Function that decreases the amount of lives Pacman has in the GameState.
-- Whenever he has no lives left the status of the game is changed to GameOver.
decreaseLives :: GameState -> GameState
decreaseLives gstate@GameState{score = currentScore, status = gamestatus, pacman = Player{playerLives, playerPosition, playerDirection, playerSpeed}, blinky = Ghost{ghostDirection, ghostPosition}} = 
  gstate{pacman = Player{playerLives = playerLives - 1, playerPosition = (448,240), playerDirection, playerSpeed}, blinky = Ghost{ghostDirection, ghostPosition = (448,624)}}

-- //
-- // CODE THAT HAS TO DO WITH THE PATHFINDING OF THE GHOSTS //
-- //

-- This function uses the getSurroundigFields function defined in Model.hs to get the new direction of a Ghost when he gets to an intersection.
-- The function is handed a targetTile, which corresponds to a grid in the Maze and the GameState and uses these to return the new Direction.
-- For a detailed description of getSurroundigFields see Model.hs
-- What this function does is calculate the distance from all fields to the targetTile and return the Direction of the field that puts the Ghost
-- closest to his targetTile. The possible fields are sorted based on Direction, because there is a preference in Direction if 2 fields have the 
-- same distance to the targetTile. This preference goes up > left > down > right and is also the reason why the Direction data type derives Ord.
determineDirection :: Point -> GameState -> Direction
determineDirection targetTile GameState{blinky = Ghost{ghostPosition = (x,y), ghostDirection = dir}} = possibleDirections !! indexOfShortestDistance
  where distances = map (calculateDistance targetTile) possiblePoints 
        shortestDistance = minimum distances
        indexOfShortestDistance = fromMaybe 0 (elemIndex shortestDistance distances)
        possiblePoints = map trd3 (sortOn snd3 (getSurroundingFields ((x + 16) / 32, 31 - ((y - 16) / 32)) dir))
        possibleDirections = map snd3 (sortOn snd3 (getSurroundingFields ((x + 16) / 32, 31 - ((y - 16) / 32)) dir))

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

-- //
-- // CODE FOR READING FROM AND WRITING TO A FILE//
-- //

-- Funtion that gets the highscore from the specified file.
getHighScore :: String -> IO String
getHighScore = readFileStrict

-- Function that gets the score at the end of the game as a parameter and uses this to
-- update the highscore file, but only when to supplied score is higher than the score
-- specified in the file.
updateHighScore :: String -> String -> IO ()
updateHighScore score file = do s <- getHighScore file
                                when ((read s :: Int) < (read score :: Int)) $ writeFile file score

-- Below methodes taken from https://github.com/nh2/shake/blob/e0c4bda9943bfadc9383ec31cfe828d67879e8ca/Development/Shake/Derived.hs to circumvent the "open file resource busy (file is locked)" error
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s

readFileStrict :: FilePath -> IO String
readFileStrict name =  openFile name ReadMode >>= hGetContentsStrict
