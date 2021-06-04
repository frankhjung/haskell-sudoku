{-|

Module      : Sudoku
Description : Simple Sudoku solver by Richard Bird
Copyright   : © Frank Jung, 2021
License     : GPL-3

= Introduction

This is an implementation of Richard Bird's Sudoku Solver from
<https://dl.acm.org/doi/book/10.5555/1951654 Pearls of Functional Algorithm Design>.

= Method

This solver essentially works at the row level. In order to work with columns
and 3 x 3 internal boxes of the grid as rows, then they first need to be
transformed into rows. Then they need to be restored. This is possible due
to the properties (see "Sudoku#properties") that these transformations have.

The work done on each row (columns and boxes) is:

  * fill unknown cells with all 'choices' of available 'digits'
  * remove fixed digits from these cell choices
  * find the first cell with a minimum (more than 1) choices
  * expand that single cell into matching choice matrices
  * recurse into each expanded 'Matrix', searching for next minium cell to prune & expand,
  * stop when either an invalid 'Matrix' or a solution 'Matrix' containing only 'singleton' cells is found.

= Properties #properties#

The following properties apply to 'rows', 'cols', 'boxs', 'group' and
'ungroup':

== Rows

prop> rows ∘ rows = id

That is:

prop> rows (rows a) = a

== Columns

prop> cols ∘ cols = id

That is:

prop> cols (cols a) = a

Where cols transposes columns to rows:

@
cols |1 2 3|   |1 4 7|
     |4 5 6| = |2 5 8|
     |7 8 9|   |3 6 9|
@

== Boxes

prop> boxs ∘ boxs = id

That is:

prop> boxs (boxs a) = a

=== Example

@
cols | 1  2  3  4|   |  1  2 |  5  6 |
     | 5  6  7  8| = |_ 3_ 4_|_ 7_ 8_|
     | 9 10 11 12|   |  9 10 | 13 14 |
     |13 14 15 17|   |_11_12_|_15_17_|
@

== Group

prop> ungroup ∘ group = id

= References

  * <https://dl.acm.org/doi/book/10.5555/1951654 Pearls of Functional Algorithm Design>
  * <https://en.wikipedia.org/wiki/Sudoku Wikipedia Sudoku>
  * <https://youtu.be/glog9DZh8G0 Sudoku I: First Steps>
  * <https://youtu.be/O1-ruHzabAU Sudoku II: Initial Solvers>
  * <https://youtu.be/ESDpXBd1cJM Sudoku III: Improving Performance>

-}

module Sudoku ( Grid
              , Matrix
              , Row
              , Cell
              , Choices
              , digits
              , solve
              , search
              , choices
              , safe
              , complete
              , expand1
              , ok
              , counts
              , rows
              , cols
              , boxs
              , group
              , ungroup
              , unknown
              , singleton
              , nodups
              ) where

import           Data.List (transpose, (\\))

-- | Representation of a Sudoku puzzle.
type Grid = Matrix Cell
-- | Generalised matrix is a list of rows.
type Matrix a = [Row a]
-- | Generalised row is a list of elements.
type Row a = [a]
-- | A cell is single character.
type Cell = Char
-- | Choices is a list of 'Cell's.
type Choices = [Cell]

-- | Restriction of 'Cell' values in a 'Matrix'.
digits :: String
digits = ['1'..'9']

-- | Sudoku puzzle solver.
solve :: Grid -> [Grid]
solve = search . choices

-- | Search for valid solutions given 'Matrix' of 'Choices'.
search :: Matrix Choices -> [Grid]
search m
  | not (safe m) = []
  | complete m'  = [map (map head) m']
  | otherwise    = concatMap search (expand1 m')
  where m' = prune m

-- | This returns 'unknown' cells filled in with all 'digits'.
choices :: Grid -> Matrix Choices
choices = map (map choice)
          where choice v = if unknown v then digits else [v]

-- | A 'Matrix' is safe if there are no duplicates in any of
-- 'rows', 'cols' or 'boxs'.
safe :: Eq a => [Matrix a] -> Bool
safe m = all ok (rows m) && all ok (cols m) && all ok (boxs m)

-- | A 'Matrix' is complete if it only contains singleton cells.
complete :: Matrix Choices -> Bool
complete = all (all singleton)

-- | Expand the smallest (see 'counts') single cell of 'Matrix'
-- of 'Choices'.
expand1 :: Matrix Choices -> [Matrix Choices]
expand1 mc = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
  where
    (rows1, row:rows2) = break (any smallest) mc
    (row1, cs:row2)    = break smallest row
    smallest           = (==n) . length
    n                  = minimum (counts mc)

-- | A 'Matrix' is OK if it contains no duplicates in any cell.
ok :: Eq a => [Row a] -> Bool
ok cells = nodups [d | [d] <- cells]

-- | Get the lengths of choice cells in a 'Matrix' of 'Choices'.
counts :: Matrix Choices -> [Int]
counts = filter (/=1) . map length . concat

-- | Retreive rows (as rows).
rows :: Matrix a -> [Row a]
rows = id

-- | Retreive columns as rows.
cols :: Matrix a -> [Row a]
cols = transpose

-- | Retrieve Sudoku boxes from the grid as rows.
boxs :: Matrix a -> [Row a]
boxs = map ungroup . ungroup . map cols . group . map group

-- | Group 3 cell boxes from grid.
group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

-- | Ungroup boxes restores grouped boxes.
ungroup :: [[a]] -> [a]
ungroup = concat

-- | Cell that contain @'0'@ are 'unknown'.
--
-- Incomplete Sudoku grids use the @'0'@ value to indicate
-- an 'unknown' value. Valid cell values are 'digits'.
unknown :: Char -> Bool
unknown = (=='0')

-- | Prune invalid 'Cell' values from a 'Matrix' of 'Choices'.
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- | Reduce choices by pruning redundent cell values.
pruneBy :: ([[Choices]] -> [[Choices]]) -> [[Choices]] -> [[Choices]]
pruneBy f = f . map pruneRow . f

-- | Prune fixed 'digits' from 'Cell' 'choices'.
pruneRow :: [Choices] -> [Choices]
pruneRow row = map (remove fixed) row
               where fixed = [d | [d] <- row]

-- | Remove fixed digits from 'Cell' of 'choices'.
remove :: Choices -> Choices -> Choices
remove xs ds = if singleton ds then ds else ds \\ xs

-- | Test if value in 'Cell' a single value.
singleton :: Foldable t => t a -> Bool
singleton = (==1) . length

-- | Remove duplicates from a choice 'Cell's.
--
-- Duplicates in 'Choices' are 'digits' from /fixed/ or 'singleton' cells.
nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = notElem x xs && nodups xs
