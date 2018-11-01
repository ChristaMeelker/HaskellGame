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

-- This Method draws a MazeField on the right position of the grid
drawField :: ((Float, Float), MazeField) -> Picture
drawField ((x,y),MazeField a b)
  | a == Wall     = translate (-250 + y*20) (-250 + x*20) $ color blue $ rectangleSolid 20 20
  | b == FoodDot  = translate (-250 + y*20) (-250 + x*20) $ color white $ circleSolid 3
  | otherwise     = color green $ circleSolid 6 -- TO DO: this last row has no meaning

-- //BUILDING FIRST LEVEL MAZE//

-- Wall
w :: MazeField
w = MazeField {field = Wall, content = Empty}

-- Straightaway with FoodDot
st :: MazeField
st = MazeField {field = Straightaway, content = FoodDot}

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
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, is, st, st, st, st, is, st, st, is, st, st, is, st, st, is, st, st, is, st, st, is, st, st, st, st, is, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  st, w],  
              [w, st, w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  st, w],
              [w, st, st, st, st, st, is, w,  w,  st, st, st, st, w,  w,  st, st, st, st, w,  w,  is, st, st, st, st, st, w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, es, es, es, es, es, es, es, es, es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  w,  w,  w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  w,  w,  w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  is, st, st, ei, w,  w,  w,  w,  w,  w,  w,  w,  ei, st, st, is, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  w,  w,  w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  es, w,  w,  w,  w,  w,  w,  w,  w,  es, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  ei, es, es, es, es, es, es, es, es, ei, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w],
              [w, st, st, st, st, st, is, st, st, is, st, st, st, w,  w,  st, st, st, is, st, st, is, st, st, st, st, st, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, st, w,  w,  w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  st, w,  w,  w,  w,  st, w],
              [w, st, st, st, w,  w,  is, st, st, is, st, st, st, st, st, st, st, st, is, st, st, is, w,  w,  st, st, st, w],
              [w, w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w],
              [w, w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  st, w,  w,  w],
              [w, st, st, is, st, st, st, w,  w,  st, st, st, st, w,  w,  st, st, st, st, w,  w,  st, st, st, is, st, st, w],
              [w, st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w],
              [w, st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w,  w,  st, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  st, w],
              [w, st, st, st, st, st, st, st, st, st, st, st, is, st, st, is, st, st, st, st, st, st, st, st, st, st, st, w],
              [w, w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w,  w]]
            

