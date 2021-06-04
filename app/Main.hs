{-| Sudoku solver main function.

Run example Sudoku solver agains puzzle read from a file:

  cabal exec sudoku -- data/easy.sudoku

 -}

module Main (main) where

import           Sudoku             (Grid, solve)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

-- True if path to puzzle file provided.
hasPuzzle :: [String] -> Bool
hasPuzzle args = length args == 1

-- Usage with current program name and command arguments.
usage :: IO ()
usage = putStrLn $
          "Usage: sudoku [puzzle]\n" ++
          "Solve Sudoku puzzle read from file."

-- Solve Sudoku puzzle provided by a file.
main :: IO ()
main = do
  args <- getArgs
  if hasPuzzle args
    then do
      ms <- readFile (head args :: FilePath)
      mapM_ putStrLn $ (head . solve) (lines ms :: Grid)
    else
      usage >> exitFailure

