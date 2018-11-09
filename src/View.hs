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
viewPure gstate@GameState{score = currentScore, pacman = Player{playerDirection = dir, playerPosition = (x, y), playerLives = lives}} 
  | dir == FaceUp    = pictures $ firstLevelDrawing ++ [picturePacManUp] ++ [pointsHeader] ++ [highScoreHeader] ++ [livesHeader] ++ [livesText] ++ [pointsText]
  | dir == FaceDown  = pictures $ firstLevelDrawing ++ [picturePacManDown] ++ [pointsHeader] ++ [highScoreHeader] ++ [livesHeader] ++ [livesText] ++ [pointsText]
  | dir == FaceLeft  = pictures $ firstLevelDrawing ++ [picturePacManLeft] ++ [pointsHeader] ++ [highScoreHeader] ++ [livesHeader] ++ [livesText] ++ [pointsText]
  | dir == FaceRight = pictures $ firstLevelDrawing ++ [picturePacManRight] ++ [pointsHeader] ++ [highScoreHeader] ++ [livesHeader] ++ [livesText] ++ [pointsText]
    where picturePacManUp = uncurry translate (-448 + x,-512 + y) $ color yellow $ thickCircle 7 18
          picturePacManDown = uncurry translate (-448 + x,-512 + y) $ color yellow $ thickCircle 7 18
          picturePacManLeft = uncurry translate (-448 + x,-512 + y) $ color yellow $ thickCircle 7 18
          picturePacManRight = uncurry translate (-448 + x,-512 + y) $ color yellow $ thickCircle 7 18
          pointsHeader = uncurry translate (-250, 540) $ color white $ scale 0.25 0.25 $ text "Score"
          pointsText = uncurry translate (-250, 500) $ color white $ scale 0.25 0.25 $ text (show currentScore)
          highScoreHeader = uncurry translate (-85, 540) $ color white $ scale 0.25 0.25 $ text "High Score"
          livesHeader = uncurry translate (165, 540) $ color white $ scale 0.25 0.25 $ text "Lives"
          livesText = uncurry translate (165, 500) $ color white $ scale 0.25 0.25 $ text (show lives)

-- pacmanX :: IO Picture
pacmandown = loadBMP "/bitmaps/pacman-down.bmp"
pacmanleft = loadBMP "/bitmaps/pacman-left.bmp"
pacmanright = loadBMP "/bitmaps/pacman-right.bmp"
pacmanup= loadBMP "/bitmaps/pacman-up.bmp"

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