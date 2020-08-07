module AIJV where

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

gameStateToScore :: State -> Maybe Score
gameStateToScore (GameOver (Just player))
  = Just (Win player)
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

findBlanks :: Board -> [(Int,Int)]
findBlanks
  = concat . mapPos
    (\pos -> maybe [pos] (const []))

score :: Game -> Score
score (Game _ _ (GameOver (Just player))) = Win player
score game@(Game board player Going) =
  case findBlanks board of
    [] -> Draw
    bs ->
      let scenarios = map (`unsafeMarkInGame` game) bs
       in maximumBy (compareByPlayer player)
          $ map score scenarios
score _ = Draw

aimove :: Game -> Maybe (Int,Int)
aimove game@(Game board player Going)
  | bs@(_:_) <- findBlanks board
  , scenarios <- map (`unsafeMarkInGame` game) bs
  = Just
  $ fst
  $ maximumBy (compareByPlayer player `on` snd)
  $ zip bs
  $ map score scenarios
aimove _ = Nothing
