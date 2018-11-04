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

-- | Handle one iteration of the game
-- Hier bepalen of volgende positie (getNextGridPosition) een muur is. Zo ja, stilstaan. Zo nee: doorlopen.
-- Eerst pauze uitwerken. Dan bij raken van muur: Pauze.
-- step is afhankelijk van speed van pacman. 

step :: Float -> GameState -> IO GameState
step secs gstate@GameState {pacman = Player {playerDirection = dir}}
      | dir == FaceUp     = return (movePacmanUp 1 gstate)
      | dir == FaceDown   = return (movePacmanDown 1 gstate)
      | dir == FaceLeft   = return (movePacmanLeft 1 gstate)
      | dir == FaceRight  = return (movePacmanRight 1 gstate)             
 
-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
-- If the user presses an arrow, update Pac-man's Location
inputKey (EventKey (SpecialKey KeyUp) Down _ _) gstate    =  movePacmanUp 1 gstate
inputKey (EventKey (SpecialKey KeyDown) Down _ _) gstate  =  movePacmanDown 1 gstate
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) gstate  =  movePacmanLeft 1 gstate
inputKey (EventKey (SpecialKey KeyRight) Down _ _) gstate =  movePacmanRight 1 gstate
-- Otherwise keep the same
inputKey _ gstate = gstate

movePacmanUp :: Float -> GameState -> GameState
movePacmanUp dy GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} = GameState{pacman = Player{playerPosition = (x, y + dy), playerDirection = FaceUp}}

movePacmanDown :: Float -> GameState -> GameState
movePacmanDown dy GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} = GameState{pacman = Player{playerPosition = (x, y - dy), playerDirection = FaceDown}}

movePacmanLeft :: Float -> GameState -> GameState
movePacmanLeft dx GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} = GameState{pacman = Player{playerPosition = (x - dx, y), playerDirection = FaceLeft}}

movePacmanRight :: Float -> GameState -> GameState
movePacmanRight dx GameState{pacman = Player{playerPosition = (x,y), playerDirection = dir}} = GameState{pacman = Player{playerPosition = (x + dx, y), playerDirection = FaceRight}}

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