{-# language NamedFieldPuns #-}

module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "Pacman" (800, 800) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function