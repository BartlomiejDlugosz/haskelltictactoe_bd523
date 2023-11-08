module Main where

import TicTacToe
import Board
import Cell
import Player
import Helpers

import Text.Read (readMaybe)
import Data.List (intercalate)

import System.IO (hSetBuffering, stdout, stdin, BufferMode(NoBuffering))

prettyPrint :: Board -> IO ()
prettyPrint b@(Board n _) = putStrLn (intercalate ("\n-" ++ concat (replicate (n - 1) "-+--") ++ "-\n") (map (intercalate " | " . map show) (rows b)))

-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- You might like to use one of the following signatures, as per the spec.
-- but this is up to you.
--
doParseAction :: String -> (String -> Maybe a) -> IO a
doParseAction prompt f = do
    putStr prompt
    s <- getLine
    case f s of
        Just b  -> return b
        Nothing -> do
            putStrLn "Invalid input. Please try again."
            doParseAction prompt f


-- | Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn original player = do
        position <- doParseAction ("Player " ++ show player ++ ". Please enter a position: ") parsePosition
        case tryMove player position original of
                Just new -> do
                        prettyPrint new
                        return new
                Nothing -> do
                        putStr "Invalid input. Please try again"
                        takeTurn original player

-- | Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame b p = do
        newBoard <- takeTurn b p
        if gameOver newBoard then 
                putStrLn "Game over" 
        else 
                playGame newBoard (swap p)


-- | Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main = do
  disableBuffering -- don't remove!
  putStrLn "Welcome to tictactoe!"
  boardSize <- doParseAction "Please enter a board size: " (readMaybe :: String -> Maybe Int)
  let b = emptyBoard boardSize
  prettyPrint b
  playGame b X
  return ()

{-|
When ran via `cabal run`, Haskell will "buffer" the input and output streams
for performance. This is annoying, since it means that some printing can happen
out-of-order -- the strings are only written when a newline is entered!

There are a few ways of getting around this, like using `hFlush stdout` after
each print. However, in this case, it is simplest to just disable the buffering
altogether. This function *must* be called at the start of your `main` function.
-}
disableBuffering :: IO ()
disableBuffering = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
