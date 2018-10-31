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
viewPure gstate | pacmanDir gstate == FaceUp       = pictures $ [maze] ++ [foodDot] ++ [picturePacManUp]   
                | pacmanDir gstate == FaceDown     = pictures $ [maze] ++ [foodDot] ++ [picturePacManDown] 
                | pacmanDir gstate == FaceLeft     = pictures $ [maze] ++ [foodDot] ++ [picturePacManLeft] 
                | pacmanDir gstate == FaceRight    = pictures $ [maze] ++ [foodDot] ++ [picturePacManRight] 
    where
        picturePacManUp :: Picture
        picturePacManUp = uncurry translate (pacmanPos gstate) $ color yellow $ Polygon [(-10,0),(10,0),(0,30)]
        picturePacManDown :: Picture
        picturePacManDown = uncurry translate (pacmanPos gstate) $ color yellow $ Polygon [(-10,0),(10,0),(0,-30)]
        picturePacManLeft :: Picture
        picturePacManLeft = uncurry translate (pacmanPos gstate) $ color yellow $ Polygon [(0,10),(0,-10),(-30,0)]
        picturePacManRight :: Picture
        picturePacManRight = uncurry translate (pacmanPos gstate) $ color yellow $ Polygon [(0,10),(0,-10),(30,0)]

maze :: Picture
maze = drawField MazeField {field = Wall, content = Empty} (2,5)

foodDot :: Picture
foodDot = drawField MazeField {field = Straightaway, content = FoodDot} (3,5)

-- TO DO:
-- Mappen over de Maze. Daarbij indexen x en y meegegeven aan drawField.