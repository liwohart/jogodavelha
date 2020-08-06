module Game where

import Data.Matrix
import Data.Maybe

data Player
    = X | O
  deriving (Show, Eq, Read, Bounded, Enum)

data State
  = Going
  | GameOver (Maybe Player)
  deriving (Show, Eq)

type Board = Matrix (Maybe Player)

data Game = Game
  { currentBoard :: Board
  , currentPlayer :: Player
  , currentState :: State
  } deriving (Show)

type Dimension = (Int, Float)

totalSide :: Float
totalSide = 600

size :: Int
size = 4

dimension :: Dimension
dimension = (size, totalSide/fromIntegral size)

-- data

initialBoard :: Int -> Board
initialBoard n = fromList n n $ repeat Nothing

initial :: Int -> Game
initial n = Game (initialBoard n) X Going

-- -tests

allXs :: Int -> Board
allXs n = matrix n n $ const $ Just X

allOs :: Int -> Board
allOs n = matrix n n $ const $ Just O


