import System.Environment (getArgs)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Matrix

import Game
import Drawing
import Logic

window = InWindow "Jogo Da Velha" (truncate totalSide,truncate totalSide) (100,50) 

main :: IO ()
main = do
  size <- read . head <$> getArgs
  play
    window
    white
    30
    (Game (allXs size) X $ GameOver $ Just X)
    (drawGame
      (size, totalSide / fromIntegral size))
    transform
    (const id)