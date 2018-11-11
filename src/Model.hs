-- | This module contains the data types
--   which represent the state of the game

{-# language NamedFieldPuns #-}
{-# language FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Model where

import Graphics.Gloss
import System.Random
import System.IO
import Control.Monad
import Control.Lens hiding (Empty)
import Data.List
import Data.List.Split

data GameState = GameState { pacman :: Player, blinky :: Ghost, pinky :: Ghost, inky :: Ghost, clyde :: Ghost, maze :: Maze, score :: Int, status :: GameStatus}
  deriving (Show)

data GameStatus = GameOn | GameLost | GameWon | GamePaused
  deriving (Eq, Show)

data Player = Player { playerPosition :: Point, playerDirection :: Direction, playerStatus :: PlayerStatus, playerSpeed :: Float, playerLives :: Int }
  deriving (Show)

data Ghost = Ghost { ghostPosition :: Point, ghostDirection :: Direction, ghostMode :: GhostMode, ghostSpeed :: Float }
  deriving (Show)

data PlayerStatus = Neutral | Energized
  deriving (Show)

data GhostMode = Chase | Scatter | Frightened
  deriving (Show)

type Maze = [MazeRow]

type MazeRow = [MazeField]

data MazeField = MazeField { field :: FieldType, content :: ContentType }
  deriving (Eq, Show)

data FieldType = Straightaway | Intersection | Wall | GhostWall
  deriving (Eq, Show)

data ContentType = FoodDot | Energizer | Empty
  deriving (Eq, Show)

data Direction = FaceUp | FaceLeft | FaceDown | FaceRight
  deriving (Eq, Show, Ord)

instance Num Point where
  (x0, y0) - (x1, y1) = (x0 - x1, y0 - y1)
  (x0, y0) + (x1, y1) = (x0 + x1, y0 + y1)

-- Constant that defines the initial state of the game.
initialState :: GameState
initialState = GameState (Player (48,896) FaceRight Neutral 3 3) (Ghost (240,314) FaceUp Chase 3) (Ghost (12,18) FaceUp Chase 3) (Ghost (14,18) FaceUp Chase 3) (Ghost (16,18) FaceUp Chase 3) firstLevel 0 GameOn

{-
// SOME INFO ABOUT MAZE/GRID/PACMAN-POSITION STRUCTURE
A regular x y GRID looks like this: Origin is lower left
y 3
  2
  1
  0 1 2 3
        x

The MAZE, however, is build with the [[]] structure. Meaning that it looks like this: Origin is upper left

       row
c  0 1 2 3
o  1
l  2
l  3

Drawing in Haskell is middle centred, which is why the coordinates in the drawing functions have to be converted
      2
      1
-2 -1 0 1 2
     -1
     -2

The following functions convert grid(x,y) to maze(row,columns)
-}

-- Function that returns position of Pacman in the grid.
getGridPosition :: Point -> Point
getGridPosition (x,y) = toPoint(floor (x/32),floor (y/32))

-- Function that returns column and row in maze based on position in the grid
getMazeCoordinates :: Point -> Point
getMazeCoordinates (x,y) = (x, abs(y-30)) 

-- This function combines the above two. To go straight from playerPosition (as in GameState) to Maze coordinates.
playerLocationInMaze :: Point -> Point
playerLocationInMaze (x,y) = getMazeCoordinates (getGridPosition (x,y))

-- This function checks what the Mazefield over 16px is. This is useful, because pacman has a radius of 16px 
fieldIn16 :: GameState -> MazeField
fieldIn16 GameState{pacman = Player {playerPosition = (x,y), playerDirection = dir}}
  | dir == FaceUp     = getMazeField (playerLocationInMaze(x,y+16)) firstLevel
  | dir == FaceDown   = getMazeField (playerLocationInMaze(x,y-16)) firstLevel
  | dir == FaceLeft   = getMazeField (playerLocationInMaze(x-16,y)) firstLevel
  | dir == FaceRight  = getMazeField (playerLocationInMaze(x+16,y)) firstLevel

-- This function checks what the Mazefield over 16px is. This is useful, because pacman has a radius of 16px 
fieldIn16Ghost :: GameState -> MazeField
fieldIn16Ghost GameState{blinky = Ghost {ghostPosition = (x,y), ghostDirection = dir}}
  | dir == FaceUp     = getMazeField (playerLocationInMaze(x,y+16)) firstLevel
  | dir == FaceDown   = getMazeField (playerLocationInMaze(x,y-16)) firstLevel
  | dir == FaceLeft   = getMazeField (playerLocationInMaze(x-16,y)) firstLevel
  | dir == FaceRight  = getMazeField (playerLocationInMaze(x+16,y)) firstLevel

-- This function deletes the FoodDot on the given location of a maze
eatFoodDot :: Point -> Maze -> Maze
eatFoodDot (x,y) level = chunksOf 28 newMaze
  where concattedLevel = concat level
        newMaze = (element (round(y*28+x)) .~ MazeField{field = Straightaway, content = Empty}) concattedLevel

--Function that returns a MazeField and its info from the maze when given a column and row
getMazeField :: Point -> Maze -> MazeField
getMazeField (column,row) maze = (maze !! rowInt) !! columnInt
  where
    rowInt = round row
    columnInt = round column

-- Function to filter out all Fields that don't have the Straightaway FieldType
filterField :: [MazeField] -> [MazeField]
filterField = filter (\getField -> field getField == Straightaway)

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

snd3 :: (a, b, c) -> b
snd3 (_,y,_) = y

trd3 :: (a, b, c) -> c
trd3 (_,_,z) = z

toPoint :: (Int,Int) -> Point
toPoint (x,y) = (fromIntegral x :: Float, fromIntegral y :: Float)

-- Function that gathers all the surrounding MazeFields. To be used at an intersection.
-- Always returns fields in the order left,front,right as seen from the direction of the Ghost.
getSurroundingFields :: Point -> Direction -> [(MazeField, Direction, Point)]
getSurroundingFields (column, row) dir
  | dir == FaceUp = filter ((\getField -> field getField == Straightaway).fst3) upFields
  | dir == FaceDown = filter ((\getField -> field getField == Straightaway).fst3) downFields
  | dir == FaceLeft = filter ((\getField -> field getField == Straightaway).fst3) leftFields 
  | otherwise = filter ((\getField -> field getField == Straightaway).fst3) rightFields
    where upFields = zip3 ([getMazeField (column - 2, row - 1) firstLevel] ++ [getMazeField (column - 1, row - 2) firstLevel] ++ [getMazeField (column, row - 1) firstLevel]) [FaceLeft, FaceUp, FaceRight] [(column - 1, row), (column, row - 1), (column + 1, row)]
          downFields = zip3 ([getMazeField (column, row - 1) firstLevel] ++ [getMazeField (column - 1, row) firstLevel] ++ [getMazeField (column - 2, row - 1) firstLevel]) [FaceRight, FaceDown, FaceLeft] [(column + 1, row), (column, row + 1), (column - 1, row)]
          leftFields = zip3 ([getMazeField (column - 1, row) firstLevel] ++ [getMazeField (column - 2, row - 1) firstLevel] ++ [getMazeField (column - 1, row - 2) firstLevel]) [FaceDown, FaceLeft, FaceUp] [(column, row + 1), (column - 1, row), (column, row - 1)]
          rightFields = zip3 ([getMazeField (column - 1, row - 2) firstLevel] ++ [getMazeField (column, row - 1) firstLevel] ++ [getMazeField (column - 1, row) firstLevel]) [FaceUp, FaceRight, FaceDown] [(column, row - 1), (column + 1, row), (column, row + 1)]

-- Function to generate a random IO Int in a given range
getRandomNumber :: Int -> Int -> IO Int
getRandomNumber a b = randomRIO (a, b)

-- Function that is given a list of MazeFields that er all Straightaway and returns a random MazeField
-- to be used by Ghosts when they are in their Frightened mode.
-- TODO: We houden nu niet de coördinaten bij van elke MazeField, maar de Ghost moet wel weten welke
-- kant hij op moet, dus deze functie moet een nieuwe direction of toch een coördinaat in het Maze returnen
-- ipv alleen mar een MazeField
{-
getRandomField :: (Int,Int) -> GameState -> IO MazeField
getRandomField (column, row) gstate = do number <- getRandomNumber 0 upperBound
                                         return (fields !! number)
  where fields = getSurroundingFields (column,row) gstate
        upperBound = length fields
-}

-- Ik heb geen idee hoe ik Random moet gebruiken voor wat ik wil, dus deze functie moeten we dan maar
-- gebruiken om het pad van Ghosts te bepalen als ze in Scatter mode zitten
{-
getScatterField :: (Int,Int) -> Direction -> MazeField
getScatterField (column, row) dir = getSurroundingFields (column,row) dir !! 1
-}
-- This function checks whether a MazeField contains a FoodDot
hasFoodDot :: MazeField -> Bool
hasFoodDot MazeField{content = x} 
    | x == FoodDot    = True
    | otherwise       = False

-- This functions counts the number of FoodDots in the maze
numberOfFoodDots :: Maze -> Int
numberOfFoodDots maze = length $ filter hasFoodDot (concat maze)


-- //BUILDING FIRST LEVEL MAZE//

-- Constant the defines a MazeField with FieldType Wall and contentType Empty.
w :: MazeField
w = MazeField {field = Wall, content = Empty}

-- Constant the defines a MazeField with FieldType GhostWall and contentType Empty.
gw :: MazeField
gw = MazeField {field = GhostWall, content = Empty}

-- Constant the defines a MazeField with FieldType Straightaway and contentType FoodDot.
st :: MazeField
st = MazeField {field = Straightaway, content = FoodDot}

-- Constant the defines a MazeField with FieldType Straightaway and contentType Energizer.
se :: MazeField
se = MazeField {field = Straightaway, content = Energizer}

-- Constant the defines a MazeField with FieldType Straightaway and contentType Empty.
es :: MazeField
es = MazeField {field = Straightaway, content = Empty}

-- Constant the defines a MazeField with FieldType Intersection and contentType FoodDot.
is :: MazeField
is = MazeField {field = Intersection, content = FoodDot}

-- Constant the defines a MazeField with FieldType Intersection and contentType Empty.
ei :: MazeField
ei = MazeField {field = Intersection, content = Empty}

-- Constant that uses above defined definitions to build the Maze.
firstLevel :: Maze
firstLevel = [[w, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w],
              [w, st, st, st, st, st, is, st, st, st, st, st, st, w,  w,  st, st, st, st, st, st, st, st, st, st, st, st, w],        
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, se, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  se, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, is, st, st, st, st, is, st, st, is, st, st, is, st, st, is, st, st, is, st, st, is, st, st, st, st, is, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  st, w],  
              [w, st, w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  st, w],
              [w, st, st, st, st, st, is, w,  w,  st, st, st, st, w,  w,  st, st, st, st, w,  w,  is, st, st, st, st, st, w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  es, w,  w,  es, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  es, w,  w,  es, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, es, es, es, es, es, es, es, es, es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  gw, gw, w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  es, es, es, es, es, es, w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  is, es, es, ei, w,  es, es, es, es, es, es, w,  ei, es, es, is, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  es, es, es, es, es, es, w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  w,  w,  w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  ei, es, es, es, es, es, es, es, es, ei, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  w,  w,  w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  w,  w,  w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, st, st, st, st, st, is, st, st, is, st, st, st, w,  w,  st, st, st, is, st, st, is, st, st, st, st, st, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, se, st, st, w,  w,  is, st, st, is, st, st, st, es, es, st, st, st, is, st, st, is, w,  w,  st, st, se, w],
              [w, w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w],
              [w, w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w],
              [w, st, st, is, st, st, st, w,  w,  st, st, st, st, w,  w,  st, st, st, st, w,  w,  st, st, st, is, st, st, w],
              [w, st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w],
              [w, st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w],
              [w, st, st, st, st, st, st, st, st, st, st, st, is, st, st, is, st, st, st, st, st, st, st, st, st, st, st, w],
              [w, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w]]
            

