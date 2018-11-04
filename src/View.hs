-- | This module defines how to turn
--   the game state into a picture

{-# language NamedFieldPuns #-}

module View where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate@GameState{pacman = Player{playerDirection = dir, playerPosition = (x, y)}} | dir == FaceUp    = pictures $ firstLevelDrawing ++ [picturePacManUp]
                                                                                           | dir == FaceDown  = pictures $ firstLevelDrawing ++ [picturePacManDown]
                                                                                           | dir == FaceLeft  = pictures $ firstLevelDrawing ++ [picturePacManLeft]
                                                                                           | dir == FaceRight = pictures $ firstLevelDrawing ++ [picturePacManRight]
    where
        picturePacManUp :: Picture
        picturePacManUp = uncurry translate (x,y) $ color yellow $ Polygon [(-10,0),(10,0),(0,30)]
        picturePacManDown :: Picture
        picturePacManDown = uncurry translate (x,y) $ color yellow $ Polygon [(-10,0),(10,0),(0,-30)]
        picturePacManLeft :: Picture
        picturePacManLeft = uncurry translate (x,y) $ color yellow $ Polygon [(0,10),(0,-10),(-30,0)]
        picturePacManRight :: Picture
        picturePacManRight = uncurry translate (x,y) $ color yellow $ Polygon [(0,10),(0,-10),(30,0)]

-- //TURN MAZE INTO PICTURES//

-- Function to make a grid of length a x b
grid :: (Num a, Num b, Enum a, Enum b) => a -> b -> [(a, b)]
grid a b = [(x, y)| x <- [0..a-1], y <-[0..b-1]]

-- Make grid for the first level
firstLevelGrid = grid 31 28

-- Concat the firstLevel maze
concatMaze = reverse (concat firstLevel)

-- Zip the maze with the grid
gridMaze = zip firstLevelGrid concatMaze

-- Draw each field in the maze.
firstLevelDrawing = map drawField gridMaze