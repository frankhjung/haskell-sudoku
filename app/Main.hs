{-| Sudoku solver main function.

 == Example Sudoku puzzle:

 @
 cat data/easy.puzzle | runhaskell app/Main.hs
 @

 -}

module Main (main) where

import           Sudoku             (Grid, solve)
import qualified System.Environment as Env (getArgs)
import qualified System.Exit        as Sys (exitFailure)

-- Valid if path to puzzle file provided.
isValid :: [String] -> Bool
isValid args = length args == 1

-- Usage with current program name and command arguments.
usage :: IO ()
usage = putStrLn $
          "Usage: sudoku [puzzle]\n" ++
          "Solve Sudoku puzzle read from file."

-- Solve Sudoku puzzle provided by a file.
main :: IO ()
main = do
  args <- Env.getArgs
  if isValid args
    then do
      ms <- readFile (head args :: FilePath)
      mapM_ putStrLn $ (head . solve) (lines ms :: Grid)
    else do
      usage
      Sys.exitFailure
