{-|
Sudoku solver main function.

Run example Sudoku solver agains puzzle read from a file:

  stack exec sudoku -- data/easy.sudoku
 -}

module Main (main) where

import           Data.Version       (showVersion)
import           Paths_sudoku       (version)
import           Sudoku             (Grid, solve)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

-- | Usage message.
usage :: [String]
usage = [ "Usage: sudoku [puzzle]"
        , "Solve Sudoku puzzle read from file."
        , "Version: " ++ showVersion version
        ]

-- Solve Sudoku puzzle provided by a file.
main = do
  args <- getArgs
  if length args == 1
    then do
      ms <- readFile (head args :: FilePath)
      mapM_ putStrLn $ (head . solve) (lines ms :: Grid)
    else
      print (unlines usage) >> exitFailure
