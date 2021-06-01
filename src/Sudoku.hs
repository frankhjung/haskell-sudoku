{-|

Module      : Sudoku
Description : Simple Sudoku solver by Richard Bird
Copyright   : © Frank Jung, 2021
License     : GPL-3

= Introduction

This is an implementation of Richard Bird's Sudoku Solver from
<https://dl.acm.org/doi/book/10.5555/1951654 Pearls of Functional Algorithm Design>.

== References

  * <https://dl.acm.org/doi/book/10.5555/1951654 Pearls of Functional Algorithm Design>.
  * <https://en.wikipedia.org/wiki/Sudoku Wikipedia Sudoku>
  * <https://youtu.be/glog9DZh8G0 AFP 2 - Sudoku I: First Steps>
  * <https://youtu.be/O1-ruHzabAU AFP 3 - Sudoku II: Initial Solvers>

-}

module Sudoku (Grid(..)
              , Matrix(..)
              , Row(..)
              , Cell(..)
              , rows
              , cols
              , boxs
              , valid
              ) where

import           Data.List (transpose)

type Grid = Matrix Cell
type Matrix a = [Row a]
type Row a = [a]
type Cell = Char

-- Restrictions on Matrix data:
-- digits = ['1'..'9']
-- blank = (=='0')

-- As @rows m = m@ we can simplify this to `id`.
-- Property:
--    @rows .rows = id@
-- i.e.
--    @rows (rows a) = a@
rows :: Matrix a -> [Row a]
rows = id

-- cols |1 2 3|   |1 4 7|
--      |4 5 6| = |2 5 8|
--      |7 8 9|   |3 6 9|
-- Property:
--    @cols . cols = id@
-- i.e.
--    @cols (cols a) = a@
cols :: Matrix a -> [Row a]
cols = transpose

-- cols | 1  2  3  4|   |  1  2  5  6 |
--      | 5  6  7  8| = |  3  4  7  8 |
--      | 9 10 11 12|   |  9 10 13 14 |
--      |13 14 15 17|   | 11 12 15 17 |
-- Property:
--    @boxs . boxs = id@
-- i.e.
--    @boxs (boxs a) = a@
boxs :: Matrix a -> [Row a]
boxs = map ungroup . ungroup . map cols . group . map group

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = notElem x xs && nodups xs

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)
