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

data FieldType = Straightwaway | Intersection | Wall

data ContentType = FoodDot | Energizer | Empty

data Direction = FaceUp | FaceDown | FaceLeft |FaceRight
  deriving (Eq, Enum, Bounded)

initialState :: GameState
initialState = GameState (4,7) FaceRight (2,10) (10,7)

firstRow :: MazeRow
firstRow = [MazeField {field = Wall, content = Empty}, MazeField {field = Wall, content = Empty}, MazeField {field = Wall, content = Empty}]