import qualified Data.Sequence as S

-- Setup for game uses PM's code for Tic-Tac-Toe from HW 3
-- https://github.com/grinnell-cs/395-homework-03

-- Movement options for the player
data Movement = InputUp | InputDown | InputLeft | InputRight deriving (Eq, Show)

-- Marks that determine what is on each space in the game
data Mark = Body | Food deriving (Eq, Show)

type Loc = (Int, Int)
type GameSpace = (Mark, Loc)

-- Position is a snapshot of the current state of the game
type Position = [GameSpace]
boardRows = 8
boardCols = 8

-- Since randomly generating a starting board is annoying,
-- we'll keep every new game the same to make things less confusing.
-- It will look like this:
--  -----------------
-- 8| | | | | | | | |
--  -----------------
-- 7| | | | | | |F| |
--  -----------------
-- 6| | | | | | | | |
--  -----------------
-- 5| | | | | | | | |
--  -----------------
-- 4| | | | | | | | |
--  -----------------
-- 3| | | |B| | | | |
--  -----------------
-- 2| | | | | | | | |
--  -----------------
-- 1| | | | | | | | |
--  -----------------
--   1 2 3 4 5 6 7 8
-- with B being the Body and F being food.
newGame :: Position
newGame = [(Body, (4,3)), (Food, (7,7))]

newSnake :: S.Seq GameSpace
newSnake = S.fromList [(Body,(4,3))]

-- Functions to help search within the snake
snakeHead :: S.Seq GameSpace -> Loc
snakeHead snake = snd (S.index snake 0)


isValidMove :: Position -> Loc -> Bool
isValidMove pos loc = (fst loc) < 0 && (fst loc) <= boardCols && (snd loc) > 0 && (snd loc) <= boardRows && ((filter (\n -> (snd n) == loc) pos) /= [(Body, loc)])


isFood :: Position -> Loc -> Bool
isFood pos loc = ((filter (\n -> (snd n) == loc) pos) == [(Food, loc)])

-- replaceFood is only called when the move is valid and the snake is
-- going to eat the food ingame. This replaces the food as the new head.
replaceFood :: Position -> Loc -> Position
replaceFood [] _ = []
replaceFood (x:xs) loc
    | (snd x) == loc = newSpace:xs
    | otherwise      = x:(replaceFood xs loc)
    where newSpace = (Body, loc)

-- generateFood takes the current frame and finds a random empty spot
-- and changes the mark to Food
generateNewFood :: Position -> Position
generateNewFood = undefined -- Will look up how to make random numbers later

-- generateFrame will take a player movement and if the movement
-- is valid, it will return Pos, otherwise returns nothing Since
-- the move loses the game.
generateFrame :: Movement -> Position -> S.Seq GameSpace -> Maybe Position
generateFrame InputUp pos snake
    | isValidMove pos newLoc == False = Nothing
    | isFood pos newLoc               = Just (replaceFood pos newLoc)
    | otherwise                       = Just (generateNewFood (toList (S.take ((S.length snake) - 1) snake)))
    where newLoc = (((fst (snakeHead snake)) + 1), (snd (snakeHead snake)))
generateFrame InputDown pos snake
    | isValidMove pos newLoc == False = Nothing
    | isFood pos newLoc               = Just (replaceFood pos newLoc)
    | otherwise                       = Just (generateNewFood (toList (S.take ((S.length snake) - 1) snake)))
    where newLoc = (((fst (snakeHead snake)) - 1), (snd (snakeHead snake)))
generateFrame InputLeft pos snake
    | isValidMove pos newLoc == False = Nothing
    | isFood pos newLoc               = Just (replaceFood pos newLoc)
    | otherwise                       = Just (generateNewFood (toList (S.take ((S.length snake) - 1) snake)))
    where newLoc = ((fst (snakeHead snake)), ((snd (snakeHead snake)) - 1))
generateFrame InputRight pos snake
    | isValidMove pos newLoc == False = Nothing
    | isFood pos newLoc               = Just (replaceFood pos newLoc)
    | otherwise                       = Just (generateNewFood (toList (S.take ((S.length snake) - 1) snake)))
    where newLoc = ((fst (snakeHead snake)), ((snd (snakeHead snake)) + 1))

-- Problems encountered so far:
-- 1. It doesn't compile for the weirdest reason.
--      "Variable not in scope: toList :: S.Seq GameSpace -> Position"
--    It should return [GameSpace] which is exactly what Position is.
--    Not really sure how to fix it. Maybe we'll talk to PM.

-- 2. There is no way to update the snake due to how Haskell works.
--    The solution would be having a function that creates a new
--    sequence for the snake by finding making a Head marking and
--    have it follow the path the snake appears to have taken. only
--    problem is that if the snake traveled along its body, or in a
--    zig-zag pattern and looks like a block of Body spaces, the function
--    won't know how to create the right path. Maybe if it takes the new
--    frame and old snake sequence it might work, but maybe not. Haven't
--    tried yet.

-- If 1 is fixed, it might compile and then we'd just need 2 fixed. If
-- you guys want to add on or fix it, go ahead.