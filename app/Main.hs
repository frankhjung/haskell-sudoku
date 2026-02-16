{-# LANGUAGE LambdaCase #-}
{-|
Sudoku solver main function.

Run example Sudoku solver against puzzle read from a file:

  stack exec sudoku -- data/easy.sudoku
 -}

module Main (main) where

import           Data.Version       (showVersion)
import           Paths_sudoku       (version)
import           Sudoku             (solve)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)

-- | Usage message.
usage :: [String]
usage = [ "Usage: sudoku [puzzle]"
        , "Solve Sudoku puzzle read from file."
        , "Version: " ++ showVersion version
        ]

-- Solve Sudoku puzzle provided by a file.
main :: IO ()
main = getArgs >>= \case
  [f] -> mapM_ putStrLn . head . solve . lines =<< readFile f
  _   -> putStrLn (unlines usage) >> exitFailure
