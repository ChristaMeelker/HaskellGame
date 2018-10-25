-- | This module defines how to turn
--   the game state into a picture

module View where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Model

view :: GameState -> IO Picture
view = return . viewPure

viewPure :: GameState -> Picture
viewPure gstate = uncurry translate (pacmanLoc gstate) $ color yellow $ circleSolid 10