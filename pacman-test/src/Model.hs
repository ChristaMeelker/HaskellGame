-- | This module contains the data types
--   which represent the state of the game

{-# language NamedFieldPuns #-}
{-# language FlexibleInstances #-}

module Model where

import Graphics.Gloss

instance Num Point where
  (x0, y0) - (x1, y1) = (x0 - x1, y0 - y1)
  (x0, y0) + (x1, y1) = (x0 + x1, y0 + y1)

data GameState = GameState { pacmanPos :: Point, pacmanDir :: Direction, blinkyPos :: Point, clydePos :: Point }

data Player = Player { playerPosition :: Point, playerStatus :: PlayerStatus }

data Ghost = Ghost { ghostPosition :: Point, ghostStatus :: GhostStatus }

data PlayerStatus = Normal | Energized

data GhostStatus = Chase | Scatter | Frightened

type Maze = [MazeRow]

type MazeRow = [MazeField]

data MazeField = MazeField { field :: FieldType, content :: ContentType }

data FieldType = Straightaway | Intersection | Wall
  deriving (Eq)

data ContentType = FoodDot | Energizer | Empty
  deriving (Eq)

data Direction = FaceUp | FaceDown | FaceLeft |FaceRight
  deriving (Eq, Enum, Bounded)

initialState :: GameState
initialState = GameState (4,7) FaceRight (2,10) (10,7)

drawField :: MazeField -> Point -> Picture
drawField (MazeField a b) (x,y)
  | a == Wall     = translate (x*20) (y*20) $ color blue $ rectangleSolid 20 20
  | b == FoodDot  = translate (x*20) (y*20) $ color white $ circleSolid 4
  | otherwise     = undefined


-- TEST MAZE AAN HET MAKEN HIER  

wall :: MazeField
wall = MazeField {field = Wall, content = Empty}

path :: MazeField
path = MazeField {field = Straightaway, content = Empty}

wallRow :: MazeRow
wallRow = [wall, wall, wall, wall, wall]

otherRow :: MazeRow
otherRow = [wall, path, path, path, wall]

testMaze :: Maze
testMaze = [wallRow, otherRow, otherRow, otherRow, wallRow]

