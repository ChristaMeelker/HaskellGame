module Main(main) where

    import Graphics.Gloss
    
    window :: Display
    window = InWindow "Nice Window" (800, 800) (10, 10)
    
    background :: Color
    background = black
    
    drawing :: Picture
    drawing = (color green (circle 100))
    
    main :: IO ()
    main = display window background drawing
    