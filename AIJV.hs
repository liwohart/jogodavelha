module AIJV where

import Graphics.Gloss.Interface.IO.Interact
import Data.Maybe
import Data.Matrix
import Data.List
import Data.Function (on)

import Game
import Logic

data Score
  = Win Player
  | Draw
  deriving (Show, Eq)

instance Ord Score where
  compare a b
    | a == b = EQ
  compare (Win X) _ = GT
  compare _ (Win X) = LT
  compare (Win O) _ = LT
  compare _ (Win O) = GT

gameStateToScore :: State -> Maybe Score
gameStateToScore (GameOver (Just player)) = Just (Win player)
gameStateToScore (GameOver Nothing) = Just Draw
gameStateToScore Going = Nothing

compareByPlayer :: Player -> Score -> Score -> Ordering
compareByPlayer _ s1 s2
  | s1 == s2 = EQ
compareByPlayer player s1 s2 =
  case (s1,s2) of
    (Win player', _)
      | player == player' -> GT
      | otherwise -> LT
    (_, Win player')
      | player == player' -> LT
      | otherwise -> GT

pickByX :: Score -> [Score] -> Score
pickByX best [] = best
pickByX _ (Win X:_) = Win X
pickByX _ (Draw:rest) = pickByX Draw rest
pickByX best (Win O: rest) = pickByX best rest

pickByO :: Score -> [Score] -> Score
pickByO best [] = best
pickByO _ (Win O:_) = Win O
pickByO _ (Draw:rest) = pickByO Draw rest
pickByO best (Win X: rest) = pickByO best rest

pickByPlayer :: Player -> [Score] -> Score
pickByPlayer X = pickByX (Win O)
pickByPlayer O = pickByO (Win X)

chooseByPlayer :: Player -> [Score] -> Score
chooseByPlayer X = maximum
chooseByPlayer _ = minimum

chooseByPlayerBy X = maximumBy
chooseByPlayerBy _ = minimumBy

findBlanks :: Board -> [(Int,Int)]
findBlanks
  = concat . mapPos
    (\pos -> maybe [pos] (const []))

sons :: Game -> [Game]
sons game = map (`unsafeMarkInGame`game) $ findBlanks $ currentBoard game

score :: Game -> Score
score (Game _ _ (GameOver (Just player))) = Win player
score game@(Game board player Going) =
  -- | isNotWorth game = Draw
  -- | otherwise =
  case findBlanks board of
    [] -> Draw
    bs ->
      let scenarios = map (`unsafeMarkInGame` game) bs
       in pickByPlayer player
          $ map score scenarios
score _ = Draw

aimove :: Game -> Maybe (Int,Int)
aimove game@(Game board player Going)
  | bs@(_:_) <- findBlanks board
  , scenarios <- map (`unsafeMarkInGame` game) bs
  = Just
  $ fst
  $ chooseByPlayerBy player (compare `on` snd)
  $ zip bs
  $ map score scenarios
aimove _ = Nothing


aitransform :: Event -> Game -> Game
aitransform (EventKey (Char 'r') Up _ _) game = initial $ nrows $ currentBoard game
aitransform (EventKey (Char 'a') Up _ _) game = maybe game (`unsafeMarkInGame` game) $ aimove game
aitransform (EventKey (MouseButton LeftButton) Up _ (mouseY, mouseX))
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
aitransform _ game = game
