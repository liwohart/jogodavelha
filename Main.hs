import System.Environment (getArgs)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Matrix
import Data.Maybe (listToMaybe, fromMaybe)

import Game
import Drawing
import Logic

window = InWindow "Jogo Da Velha" (truncate totalSide,truncate totalSide) (100,50) 

main :: IO ()
main = do
  size <- maybe 3 read . listToMaybe <$> getArgs
  play
    window
    white
    30
    (initial size)
    (drawGame
      (size, totalSide / fromIntegral size))
    transform
    (const id)
