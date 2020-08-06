module Logic where

import Graphics.Gloss.Interface.IO.Interact
import Data.Maybe
import Data.Matrix

import Game

other :: Player -> Player
other X = O
other O = X

full :: Board -> Bool
full = isJust . sequenceA 

won :: (Int,Int) -> Board -> Bool
won (i,j) board = anyWin $ boardRow:boardCol:boardDia
  where
    n = nrows board
    boardRow = [ board ! (i,index)
               | index <- [1..n]]
    boardCol = [ board ! (index,j)
               | index <- [1..n]]
    boardDia
      | inPrin && inSec = [prin, sec]
      | inPrin = [prin]
      | inSec = [sec]
      | otherwise = []
      where
        inPrin = i==j
        prin = [ board ! (index,index)
               | index <- [1..n]]
        inSec = i + j == n + 1
        sec = [ board ! (index,n + 1 - index)
              | index <- [1..n]]
    allSame = maybe False (all (uncurry (==)) . (zip <$> id <*> tail))
              . sequenceA 
    anyWin ls = any allSame ls

markInPosAs :: (Int,Int) -> Player -> Board -> Maybe Board
markInPosAs pos player board =
  case board ! pos of
    Nothing -> Just $ setElem (Just player) pos board
    _ -> Nothing

transform :: Event -> Game -> Game
transform (EventKey (MouseButton RightButton) Up _ _) game = initial $ nrows $ currentBoard game
transform (EventKey (MouseButton LeftButton) Up _ (mouseY, mouseX))
          game@(Game board player state)
  | n <- fromIntegral $ nrows board
  , i <- ceiling $ n*(mouseY + totalSide/2)/totalSide
  , j <- ceiling $ n*(mouseX + totalSide/2)/totalSide
  , mouseX >= -totalSide/2 && mouseX <= totalSide/2 && mouseY >= -totalSide/2 && mouseY <= totalSide/2
  , Going <- state
  , Just board' <- markInPosAs (i,j) player board
  = if not (won (i,j) board')
    then if not (full board')
         then Game board' (other player) Going
         else Game board' (other player) $ GameOver Nothing
    else Game board' (other player) $ GameOver $ Just player
transform _ game = game
