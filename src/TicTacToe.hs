module TicTacToe (gameOver, parsePosition, tryMove) where

import Data.List (nub)
import Text.Read (readMaybe)

import Board
import Cell
import Player
import Helpers

type Position = (Int, Int)

gameOver :: Board -> Bool
gameOver b = check (cols b) || check (rows b) || check (diags b)
        where
                check :: [[Cell]] -> Bool
                check = foldr (\x -> (||) (all (\f -> f == head x && not (isEmpty f)) x)) False

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition s = (,) <$> x' <*> y'
        where
                (x', y') = case words s of
                        [x, y] -> (readMaybe x, readMaybe y)
                        _ -> (Nothing, Nothing)

tryMove :: Player -> Position -> Board -> Maybe Board
tryMove p (x, y) (Board n b)
        | x >= 0 && x < n && y >= 0 && y < n && b!!(x * n + y) == Empty = Just (Board n (replace (x * n + y) (Taken p) b))
        | otherwise = Nothing
