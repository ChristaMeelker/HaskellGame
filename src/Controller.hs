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

{-
Waar ik naar toe wil:
Functie? Collision, die bij elke step wordt aangeroepen. 
Collision krijgt GameState en Maze mee, en past deze eventueel aan.
MAAR: Kunnen we de Maze eigenlijk wel aanpassen? 
Collision pacman en muur: playerSpeed = Stopped
Collision pacman en FoodDot: Punten omhoog en MazeField{content = Empty}
Collision pacman en Energizer: Pacman gaat sneller lopen en status van ghosts verandert.
Collision pacman en Ghost: Leven eraf
-}

step :: Float -> GameState -> IO GameState
step secs gstate@GameState {pacman = Player {playerDirection = dir, playerSpeed = speed}}
      | speed == Stopped  = return gstate
      | fieldIn16 gstate == MazeField{field = Wall, content = Empty}  = return gstate
      | dir == FaceUp     = return (movePacmanUp 1 gstate)
      | dir == FaceDown   = return (movePacmanDown 1 gstate)
      | dir == FaceLeft   = return (movePacmanLeft 1 gstate)
      | dir == FaceRight  = return (movePacmanRight 1 gstate)             
 
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- If the user presses an arrow, update Pac-man's Location
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate    =  movePacmanUp 0 gstate
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate  =  movePacmanDown 0 gstate
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate  =  movePacmanLeft 0 gstate
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate =  movePacmanRight 0 gstate
-- If the user presses F1, restart or pause game
inputKey (EventKey (SpecialKey KeyF1) Down _ _) gstate =  pauseGame gstate
-- Otherwise keep the same
inputKey _ gstate = gstate

-- This function changes the current speed of Pacman. 
-- If current speed is normal, pacman stops.
-- If current speed is stopped, pacman walks again.
pauseGame :: GameState -> GameState
pauseGame GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir, playerSpeed = speed}}
  | speed == Normal    = GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir, playerSpeed = Stopped}}
  | speed == Stopped   = GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir, playerSpeed = Normal}}
  | otherwise          = GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir, playerSpeed = speed}}

-- This function stops pacman from walking. Is used when pacman hits a wall.  
pausePacman :: GameState -> GameState
pausePacman GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir, playerSpeed = speed}} = GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir, playerSpeed = Stopped}}

-- TO DO: ALLEEN DIR AANPASSEN ALS DAT DAADWERKELIJK KAN. NIET ALS WE TEGEN EEN MUUR AAN STAAN DUS.
movePacmanUp :: Float -> GameState -> GameState
movePacmanUp dy GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} = GameState{pacman = Player{playerPosition = (x, y + dy), playerDirection = FaceUp, playerSpeed = Normal}}

movePacmanDown :: Float -> GameState -> GameState
movePacmanDown dy GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} = GameState{pacman = Player{playerPosition = (x, y - dy), playerDirection = FaceDown, playerSpeed = Normal}}

movePacmanLeft :: Float -> GameState -> GameState
movePacmanLeft dx GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} = GameState{pacman = Player{playerPosition = (x - dx, y), playerDirection = FaceLeft, playerSpeed = Normal}}

movePacmanRight :: Float -> GameState -> GameState
movePacmanRight dx GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} = GameState{pacman = Player{playerPosition = (x + dx, y), playerDirection = FaceRight, playerSpeed = Normal}}

calculateDistance :: Point -> Point -> Float
calculateDistance (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

getRandomNumber :: Int -> Int -> IO Int
getRandomNumber a b = randomRIO (a, b)

-- Right now if 2 tiles are the same distance from the target tile, the first tile
-- of these 2 is chosen, but it should be different. If two tiles have the same distance 
-- to the target tile the order should be up > left > down > right.
determinePath :: Point -> [Point] -> Point
determinePath targetTile possiblePaths = possiblePaths !! indexOfShortestDistance
  where distances = map (calculateDistance targetTile) possiblePaths 
        shortestDistance = minimum distances
        indexOfShortestDistance = fromMaybe 0 (elemIndex shortestDistance distances)

blinkyChaseTarget :: GameState -> Point
blinkyChaseTarget GameState{pacman = Player{playerPosition = (x, y)}} = (x, y)

blinkyScatterTarget :: Point
blinkyScatterTarget = (26,1)

-- Because of an overflow error in the original game, whenever Pac-man is facing upwards
-- Pinky's target is actually 4 tiles up and 4 tiles to the left, instead of the normal
-- 4 tiles infront of Pac-man. We chose not to implement this, but can be changed quite easily.
pinkyChaseTarget :: GameState -> Point
pinkyChaseTarget GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} 
  | dir == FaceUp   = (x, y - 4)
  | dir == FaceDown = (x, y + 4)
  | dir == FaceLeft = (x - 4, y)
  | otherwise       = (x + 4, y)

pinkyScatterTarget :: Point
pinkyScatterTarget = (3,1)

inkyChaseTarget :: GameState -> Point
inkyChaseTarget GameState{pacman = Player{playerPosition = (x0,y0), playerDirection = dir}, blinky = Ghost{ghostPosition = (x1, y1)}} = mirrorPoint + mirrorDelta
  where mirrorPoint | dir == FaceUp   = (x0, y0 - 2)
                    | dir == FaceDown = (x0, y0 + 2)
                    | dir == FaceLeft = (x0 - 2, y0)
                    | otherwise       = (x0 + 2, y0)
        mirrorDelta = mirrorPoint - (x1, y1)

inkyScatterTarget :: Point
inkyScatterTarget = (28,36)

clydeChaseTarget :: GameState -> Point
clydeChaseTarget GameState{pacman = Player{playerPosition = (x0,y0), playerDirection = dir}, blinky = Ghost{ghostPosition = (x1, y1)}} 
  | distancePacmanClyde > 8 = blinkyChaseTarget GameState{pacman = Player{playerPosition = (x0, y0)}}
  | otherwise = clydeScatterTarget
    where distancePacmanClyde = calculateDistance (x0, y0) (x1, y1)

clydeScatterTarget :: Point
clydeScatterTarget = (1,36)

getHighScore :: String -> IO String
getHighScore = readFileStrict

updateHighScore :: String -> String -> IO ()
updateHighScore score file = do s <- getHighScore file
                                when (s < score) $ writeFile file score

-- Below methodes taken from https://github.com/nh2/shake/blob/e0c4bda9943bfadc9383ec31cfe828d67879e8ca/Development/Shake/Derived.hs to circumvent the "open file resource busy (file is locked)" error
hGetContentsStrict :: Handle -> IO String
hGetContentsStrict h = hGetContents h >>= \s -> length s `seq` return s

readFileStrict :: FilePath -> IO String
readFileStrict name =  openFile name ReadMode >>= hGetContentsStrict