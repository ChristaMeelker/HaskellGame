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
viewPure gstate | pacmanDir gstate == FaceUp = uncurry translate (pacmanPos gstate) $ color yellow $ Polygon [(-10,0),(10,0),(0,30)]
                | pacmanDir gstate == FaceDown = uncurry translate (pacmanPos gstate) $ color yellow $ Polygon [(-10,0),(10,0),(0,-30)]
                | pacmanDir gstate == FaceLeft = uncurry translate (pacmanPos gstate) $ color yellow $ Polygon [(0,10),(0,-10),(-30,0)]
                | pacmanDir gstate == FaceRight = uncurry translate (pacmanPos gstate) $ color yellow $ Polygon [(0,10),(0,-10),(30,0)]