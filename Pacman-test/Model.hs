-- | This module contains the data types
--   which represent the state of the game
module Model where

import Graphics.Gloss

data GameState = GameState { pacmanLoc :: Point}
  deriving (Show)

data Player = Player { playerPosition :: Point, playerStatus :: PlayerStatus }

data Ghost = Ghost { ghostPosition :: Point, ghostStatus :: GhostStatus}

data PlayerStatus = Normal | Energized

data GhostStatus = Chase | Scatter | Frightened

type Maze = [MazeRow]

type MazeRow = [MazeField]

data MazeField = MazeField { field :: FieldType, content :: ContentType }

data FieldType = Wall | Junction | Straightaway

data ContentType = FoodDot | Energizer | Empty

data Direction = FaceUp | FaceDown | FaceLeft |FaceRight

initialState :: GameState
initialState = GameState (0,0)