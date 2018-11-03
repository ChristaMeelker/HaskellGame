-- | This module contains the data types
--   which represent the state of the game

{-# language NamedFieldPuns #-}
{-# language FlexibleInstances #-}

module Model where

import Graphics.Gloss
import System.Random
import System.IO
import Control.Monad
import Data.List

instance Num Point where
  (x0, y0) - (x1, y1) = (x0 - x1, y0 - y1)
  (x0, y0) + (x1, y1) = (x0 + x1, y0 + y1)

data GameState = GameState { pacmanPos :: Point, pacmanDir :: Direction, pacmanSpeed :: Speed, blinkyPos :: Point, clydePos :: Point }

data Speed = Standing | Walking
  deriving (Eq)

-- NOTE: waar gebruiken we data Player eigenlijk voor? playerPosition is hetzelfde als pacmanPos toch? Ik heb nu 
-- pacmanSpeed toegevoegd, Player lijkt mij dan overbodig.  

data Player = Player { playerPosition :: Point, playerStatus :: PlayerStatus }

data Ghost = Ghost { ghostPosition :: Point, ghostStatus :: GhostStatus }

data PlayerStatus = Normal | Energized

data GhostStatus = Chase | Scatter | Frightened

type Maze = [MazeRow]

type MazeRow = [MazeField]

data MazeField = MazeField { field :: FieldType, content :: ContentType }
  deriving (Show)

data FieldType = Straightaway | Intersection | Wall | GhostWall
  deriving (Eq, Show)

data ContentType = FoodDot | Energizer | Empty
  deriving (Eq, Show)

data Direction = FaceUp | FaceDown | FaceLeft |FaceRight
  deriving (Eq, Enum, Bounded)

initialState :: GameState
initialState = GameState (4,7) FaceRight Walking (2,10) (10,7)

-- This Method draws a MazeField on the right position of the grid
drawField :: ((Float, Float), MazeField) -> Picture
drawField ((x,y),MazeField a b)
  | a == Wall      = translate (-432 + y * 32) (-496 + x * 32) $ color blue $ rectangleSolid 32 32
  | a == GhostWall = translate (-432 + y * 32) (-496 + x * 32) $ color rose $ rectangleSolid 32 32
  | b == FoodDot   = translate (-432 + y * 32) (-496 + x * 32) $ color (makeColor 1 0.7255 0.6863 1) $ rectangleSolid 4 4
  | b == Energizer = translate (-432 + y * 32) (-496 + x * 32) $ color (makeColor 1 0.7255 0.6863 1) $ circleSolid 10
  | otherwise      = blank

-- Function that returns position of Pacman in the grid.
getGridPosition :: Point -> (Int,Int)
getGridPosition (x,y) = (loseRest $ x/32, loseRest $ y/32) 
  where loseRest x                  -- Lose everything after comma
          | x >= 0    = floor x
          | otherwise = ceiling x

getNextGridPosition :: (Int,Int) -> GameState -> (Int,Int)
getNextGridPosition = undefined

--Function that returns a MazeField and its info from the maze when given a column and row
getMazeField :: (Int,Int) -> Maze -> MazeField
getMazeField (column,row) maze = (maze !! (row - 1)) !! (column - 1)

-- Function to filter out all Fields that don't have the Straightaway FieldType
filterField :: [MazeField] -> [MazeField]
filterField fields = filter (\getField -> field getField == Straightaway) fields

-- Function to gather all the surrounding MazeFields when a Ghost gets to an intersection.
-- Always returns fields in the order left,front,right as seen from the direction of the Ghost.
getSurroundingFields :: (Int,Int) -> GameState -> [MazeField]
getSurroundingFields (column, row) GameState{pacmanDir = dir} 
  | dir == FaceUp = filterField [getMazeField (column - 1, row) firstLevel] ++ [getMazeField (column, row - 1) firstLevel] ++ [getMazeField (column + 1, row) firstLevel]
  | dir == FaceDown = filterField [getMazeField (column + 1, row) firstLevel] ++ [getMazeField (column, row + 1) firstLevel] ++ [getMazeField (column - 1, row) firstLevel]
  | dir == FaceLeft = filterField [getMazeField (column, row + 1) firstLevel] ++ [getMazeField (column - 1, row) firstLevel] ++ [getMazeField (column, row - 1) firstLevel]
  | otherwise = filterField [getMazeField (column, row - 1) firstLevel] ++ [getMazeField (column + 1, row) firstLevel] ++ [getMazeField (column, row + 1) firstLevel]

-- Function to generate a random IO Int in a given range
getRandomNumber :: Int -> Int -> IO Int
getRandomNumber a b = randomRIO (a, b)

-- Function that is given a list of MazeFields that er all Straightaway and returns a random MazeField
-- to be used by Ghosts when they are in their Frightened mode.
-- TODO: We houden nu niet de coördinaten bij van elke MazeField, maar de Ghost moet wel weten welke
-- kant hij op moet, dus deze functie moet een nieuwe direction of toch een coördinaat in het Maze returnen
-- ipv alleen mar een MazeField
getRandomField :: (Int,Int) -> GameState -> IO MazeField
getRandomField (column, row) gstate = do number <- getRandomNumber 0 upperBound
                                         return ((fields) !! number)
  where fields = getSurroundingFields (column,row) gstate
        upperBound = length fields

-- //BUILDING FIRST LEVEL MAZE//

-- Wall
w :: MazeField
w = MazeField {field = Wall, content = Empty}

-- GhostWall
gw :: MazeField
gw = MazeField {field = GhostWall, content = Empty}

-- Straightaway with FoodDot
st :: MazeField
st = MazeField {field = Straightaway, content = FoodDot}

-- Straightaway with Energizer
se :: MazeField
se = MazeField {field = Straightaway, content = Energizer}

-- Empty Straightaway
es :: MazeField
es = MazeField {field = Straightaway, content = Empty}

-- Intersection with FoodDot
is :: MazeField
is = MazeField {field = Intersection, content = FoodDot}

-- Empty Intersection
ei :: MazeField
ei = MazeField {field = Intersection, content = Empty}

firstLevel :: Maze
firstLevel = [[w, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w],
              [w, st, st, st, st, st, is, st, st, st, st, st, st, w,  w,  st, st, st, st, st, st, is, st, st, st, st, st, w],        
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, se, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  se, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, is, st, st, st, st, is, st, st, is, st, st, is, st, st, is, st, st, is, st, st, is, st, st, st, st, is, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  st, w],  
              [w, st, w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  st, w],
              [w, st, st, st, st, st, is, w,  w,  st, st, st, st, w,  w,  st, st, st, st, w,  w,  is, st, st, st, st, st, w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, es, es, es, es, es, es, es, es, es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  gw, gw, w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  es, es, es, es, es, es, w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  is, st, st, ei, w,  es, es, es, es, es, es, w,  ei, st, st, is, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  es, es, es, es, es, es, w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  w,  w,  w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  ei, es, es, es, es, es, es, es, es, ei, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, st, st, st, st, st, is, st, st, is, st, st, st, w,  w,  st, st, st, is, st, st, is, st, st, st, st, st, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, se, st, st, w,  w,  is, st, st, is, st, st, st, st, st, st, st, st, is, st, st, is, w,  w,  st, st, se, w],
              [w, w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w],
              [w, w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w],
              [w, st, st, is, st, st, st, w,  w,  st, st, st, st, w,  w,  st, st, st, st, w,  w,  st, st, st, is, st, st, w],
              [w, st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w],
              [w, st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w],
              [w, st, st, st, st, st, st, st, st, st, st, st, is, st, st, is, st, st, st, st, st, st, st, st, st, st, st, w],
              [w, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w]]
            

