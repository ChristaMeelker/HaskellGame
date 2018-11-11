-- | This module defines how to turn
--   the game state into a picture

{-# language NamedFieldPuns #-}

module View where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Model

-- Function that coverts the GameState to an IO Picture using return and viewPure so it can be used
-- in the main function in Main.hs.
view :: GameState -> IO Picture
view = return . viewPure

-- Function that is given almost the entire GameState and returns a list of pictures to be drawn on in the window.
-- The status of the game is used to switch between what has to be drawn and what doesn't.
viewPure :: GameState -> Picture
viewPure gstate@GameState{score = currentScore, pacman = Player{playerDirection = dir, playerPosition = (x, y), playerLives = lives}, blinky = Ghost{ghostPosition = (a,b), ghostDirection = bdir}, maze = level, status = status}
  | status == GamePaused  = pictures $ allGameInfoText ++ [pauseText]
  | status == GameWon     = pictures $ allGameInfoText ++ [gameWonText]
  | status == GameLost    = pictures $ allGameInfoText ++ [gameLostText]
  | otherwise             = pictures $ mazeToDrawing level ++ [pacmanPicture] ++ [blinkyPicture] ++ allGameInfoText
    where pacmanPicture = uncurry translate (-448 + x,-512 + y) $ color yellow $ thickCircle 7 18
          blinkyPicture = uncurry translate (-448 + a,-512 + b) $ color red $ thickCircle 7 15
          pointsHeader = uncurry translate (-250, 540) $ color white $ scale 0.25 0.25 $ text "Score"
          pointsText = uncurry translate (-250, 500) $ color white $ scale 0.25 0.25 $ text (show currentScore)
          livesHeader = uncurry translate (165, 540) $ color white $ scale 0.25 0.25 $ text "Lives"
          livesText = uncurry translate (165, 500) $ color white $ scale 0.25 0.25 $ text (show lives)
          allGameInfoText = [pointsHeader] ++ [livesHeader] ++ [livesText] ++ [pointsText]
          pauseText = uncurry translate (-250, 0) $ color white $ scale 0.25 0.60 $ text "Pause - Press F1 to continue"
          gameWonText = uncurry translate (-150, 0) $ color white $ scale 0.25 0.60 $ text "Congrats, you won!"
          gameLostText = uncurry translate (-150, 0) $ color white $ scale 0.25 0.60 $ text "Too bad, you lost!"         

-- //TURN MAZE INTO PICTURES//

-- This Method draws a MazeField on the right position of the grid
drawField :: ((Float, Float), MazeField) -> Picture
drawField ((x,y),MazeField a b)
  | a == Wall      = translate (-432 + y * 32) (-496 + x * 32) $ color blue $ rectangleSolid 32 32
  | a == GhostWall = translate (-432 + y * 32) (-496 + x * 32) $ color rose $ rectangleSolid 32 32
  | b == FoodDot   = translate (-432 + y * 32) (-496 + x * 32) $ color (makeColor 1 0.7255 0.6863 1) $ rectangleSolid 4 4
  | b == Energizer = translate (-432 + y * 32) (-496 + x * 32) $ color (makeColor 1 0.7255 0.6863 1) $ circleSolid 10
  | otherwise      = blank

-- Function to make a grid of length a x b
grid :: (Num a, Num b, Enum a, Enum b) => a -> b -> [(a, b)]
grid a b = [(x, y)| x <- [0..a-1], y <-[0..b-1]]

-- Make grid for the first level
firstLevelGrid :: [(Float, Float)]
firstLevelGrid = grid 31 28

-- functie die van Maze naar drawing gaat
mazeToDrawing :: Maze -> [Picture]
mazeToDrawing level = map drawField gridMaze
  where concatMaze = reverse (concatMap reverse level)
        gridMaze = zip firstLevelGrid concatMaze
