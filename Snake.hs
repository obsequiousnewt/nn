module Snake where

import qualified Data.Sequence as S
import Data.Sequence (Seq, fromList, index, elemIndexR, (<|))
import Data.Foldable
import qualified Data.Vector as V
import Data.Vector (Vector, (//))

-- Setup for game uses PM's code for Tic-Tac-Toe from HW 3
-- https://github.com/grinnell-cs/395-homework-03

-- Movement options for the player
data Movement = InputUp | InputDown | InputLeft | InputRight deriving (Eq, Show)

--           x    y
type Loc = (Int, Int)

type Position = ([Loc],Seq Loc)

boardRows = 8
boardCols = 8

-- Since randomly generating a starting board is annoying,
-- we'll keep every new game the same to make things less confusing.
-- It will look like this:
--  -----------------
-- 7| | | | | | | | |
--  -----------------
-- 6| | | | | | |F| |
--  -----------------
-- 5| | | | | | | | |
--  -----------------
-- 4| | | | | | | | |
--  -----------------
-- 3| | | | | | | | |
--  -----------------
-- 2| | | |B| | | | |
--  -----------------
-- 1| | | | | | | | |
--  -----------------
-- 0| | | | | | | | |
--  -----------------
--   0 1 2 3 4 5 6 7
-- with B being the Body and F being food.

newGame :: Position
newGame = ([(7,7)],fromList [(4,3)])

-- generates a gameboard in the format of a 1D array that can be easily read by the neural net

toGameboard :: Position -> Vector Double
toGameboard (foods,snake) = (V.replicate (boardRows * boardCols) 0) // (map (\(x,y) -> ((boardRows * y) + x,1.0)) foods) // (map (\(x,y) -> ((boardRows * y) + x,-1.0)) $ toList snake)

-- Functions to help search within the snake
snakeHead :: Seq Loc -> Loc
snakeHead snake = index snake 0

hitSelf :: Loc -> Seq Loc -> Bool
hitSelf newHead snake = case (elemIndexR newHead snake) of
    Just _  -> True
    Nothing -> False

-- generateFrame will take a player movement and if the movement
-- is valid, it will return Position, otherwise returns nothing since
-- the move loses the game.

-- returns also a bool signaling whether we have just eaten a fruit, so scoring is easier

generateFrame :: Movement -> Position -> Maybe (Bool,Position)
generateFrame InputUp (foods,snake)
    | y == boardRows-1 = Nothing
    | otherwise      = generateFrame' (x,y+1) (foods,snake)
    where (x,y)   = snakeHead snake
generateFrame InputDown (foods,snake)
    | y == 0         = Nothing
    | otherwise      = generateFrame' (x,y-1) (foods,snake)
    where (x,y)   = snakeHead snake
generateFrame InputLeft (foods,snake)
    | x == 0         = Nothing
    | otherwise      = generateFrame' (x-1,y) (foods,snake)
    where (x,y)   = snakeHead snake
generateFrame InputRight (foods,snake)
    | x == boardCols-1 = Nothing
    | otherwise      = generateFrame' (x+1,y) (foods,snake)
    where (x,y)   = snakeHead snake

generateFrame' :: Loc -> Position -> Maybe (Bool,Position)
generateFrame' newHead (foods,snake)
    | hitSelf newHead snake = Nothing
    | elem newHead foods    = Just (True,(filter (/=newHead) foods, newHead <| snake))
    | otherwise             = Just (False,(foods, newHead <| (S.take ((S.length snake) - 1) snake)))
