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

isNotWorth :: Game -> Bool
isNotWorth (Game _ _ (GameOver _)) = True
isNotWorth (Game board player Going) = all (thereIsA (other player)) totalLines
  where
    rows = toLists board
    cols = toLists $ transpose board
    totalLines = rows ++ cols
    thereIsA p = elem (Just p)

wonBy :: Player -> Board -> Bool
wonBy player board = anyWin $ concat [boardRows, boardCols, boardDias]
  where
    n = nrows board
    boardRows = toLists board
    boardCols = toLists $ transpose board
    boardDias = [prin, sec]
      where
        prin = [ board ! (index,index)
               | index <- [1..n]]
        sec = [ board ! (index,n + 1 - index)
              | index <- [1..n]]
    allSame l = maybe False (all (uncurry (==)) . (zip <$> id <*> tail))
              $ sequenceA l
    anyWin ls = any allSame ls

wonIn :: (Int,Int) -> Board -> Bool
wonIn (i,j) board = anyWin $ boardRow:boardCol:boardDia
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

markAsIn :: Player -> (Int,Int) -> Board -> Maybe Board
markAsIn player pos board =
  case board ! pos of
    Nothing -> Just $ setElem (Just player) pos board
    _ -> Nothing

unsafeMark :: Player -> (Int,Int) -> Board -> Board
unsafeMark = setElem . Just

unsafeMarkInGame :: (Int,Int) -> Game -> Game
unsafeMarkInGame pos (Game board player state)
  | wonIn pos board' = Game board' (other player) (GameOver $ Just player)
  | full board' = Game board' (other player) (GameOver Nothing)
  | otherwise = Game board' (other player) state
  where board' = unsafeMark player pos board

transform :: Event -> Game -> Game
transform (EventKey (Char 'r') Up _ _) game = initial $ nrows $ currentBoard game
transform (EventKey (MouseButton LeftButton) Up _ (mouseY, mouseX))
          game@(Game board player state)
  | n <- fromIntegral $ nrows board
  , i <- ceiling $ n*(mouseY + totalSide/2)/totalSide
  , j <- ceiling $ n*(mouseX + totalSide/2)/totalSide
  , mouseX >= -totalSide/2 && mouseX <= totalSide/2 && mouseY >= -totalSide/2 && mouseY <= totalSide/2
  , Going <- state
  , Just board' <- markAsIn player (i,j) board
  = if not (wonIn (i,j) board')
    then if not (full board')
         then Game board' (other player) Going
         else Game board' (other player) $ GameOver Nothing
    else Game board' (other player) $ GameOver $ Just player
transform _ game = game
