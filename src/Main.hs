-- | This module contains the mian function
--   used to iterate the game

module Main where

import Model
import Controller
import View

import Graphics.Gloss.Interface.IO.Game

-- Our main function consist solely of the bij Gloss defined playIO function and
-- uses out entire codebase to play the game at 60 frames per second while responding
-- to user input.
main :: IO ()
main = playIO (InWindow "Pacman" (896, 1152) (0, 0)) -- Window definition
              black            -- Background color
              60               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function
