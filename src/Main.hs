module Main(main) where

    import Graphics.Gloss
    
    window :: Display
    window = InWindow "Pacman" (600, 400) (10, 10)
    
    background :: Color
    background = black
    
    drawing :: Picture
    --drawing = (color yellow (circleSolid 50))
    drawing = pictures [translate (-20) (-100) $ color yellow (circleSolid 50), translate 20 50 $ color yellow (rectangleSolid 10 50)]
    
    main :: IO ()
    main = animate window background frame
        where
            frame :: Float -> Picture
            frame seconds = render $ movePacman seconds initialState    
  

    data PacManGame = Game {   
                        pacmanLoc :: (Float,Float),
                        pacmanVel :: (Float,Float),
                        ghostLoc :: (Float,Float)
                            } deriving Show

    initialState :: PacManGame
    initialState = Game {   
                    pacmanLoc = (-10, 30),
                    pacmanVel = (30, 1),
                    ghostLoc = (20, 50)
                        }

    render :: PacManGame -> Picture
    render game = 
        pictures [pacman, ghost]
        where 
            pacman = uncurry translate (pacmanLoc game) $ color yellow $ circleSolid 10
            
            ghost = uncurry translate (ghostLoc game) $ color ghostColor $ circleSolid 10
            ghostColor = magenta

    movePacman :: Float -> PacManGame -> PacManGame
    movePacman seconds game = game {pacmanLoc = (x', y')}
            where
                -- old locations and velocitie
                (x, y) = pacmanLoc game
                (vx, vy) = pacmanVel game

                -- new locations
                x' = x + vx * seconds
                y' = y + vy * seconds               