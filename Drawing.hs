module Drawing where

import Graphics.Gloss
import Data.Foldable
import Data.Matrix

import Game

drawHline :: Int -> Float -> Float -> Picture
drawHline n pos totalSide = translate 0 pos $ rectangleSolid totalSide (totalSide/(fromIntegral (n+1)*10))

drawVline :: Int -> Float -> Float -> Picture
drawVline n pos totalSide = translate pos 0 $ rectangleSolid (totalSide/(fromIntegral (n+1)*10)) totalSide

drawGrid :: Dimension -> Picture
drawGrid (n, side) = pictures
                   [ drawVline n ((i - nf / 2) * side) tside
                   | i <- [1 .. nf - 1]]
                   <> pictures
                   [ drawHline n ((i - nf / 2) * side) tside
                   | i <- [1 .. nf - 1]]
  where
    nf = fromIntegral n
    tside = nf * side

drawPlayer :: Float -> Player -> Picture
drawPlayer side X =
  let rect = rectangleSolid (side/8) side
   in rotate 45 rect <> rotate (-45) rect
drawPlayer side O = thickCircle (side/sqrt 2/2) (side/10)

drawBoard :: Dimension -> Board -> Picture
drawBoard d@(n, side)
    = (drawGrid d <>)
    . translate (-side * fromIntegral (n + 1) / 2) (-side * fromIntegral (n + 1) / 2)
    . fold
    . mapPos maybeDraw 
  where
    nf = fromIntegral n
    maybeDraw (i,j) = maybe Blank
                    $ translate (totalSide / nf * fromIntegral i) (totalSide / nf * fromIntegral j)
                    . drawPlayer (totalSide/ fromIntegral n)

drawGame :: Dimension -> Game -> Picture
drawGame d game = changeColor $ drawBoard d $ currentBoard game
  where changeColor =
          case currentState game of
            GameOver (Just X) -> color red
            GameOver (Just O) -> color blue
            GameOver Nothing -> color $ greyN 0.4
            _ -> id
