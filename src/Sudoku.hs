{-|

Module      : Sudoku
Description : Simple Sudoku solver by Richard Bird
Copyright   : © Frank Jung, 2021
License     : GPL-3

= Introduction

This is an implementation of Richard Bird's Sudoku Solver from
<https://dl.acm.org/doi/book/10.5555/1951654 Pearls of Functional Algorithm Design>.

== Properties

=== Rows

@rows ∘ rows = id@

That is:

@rows (rows a) = a@

=== Columns

@cols ∘ cols = id@

That is:

   @cols (cols a) = a@

Where cols transposes columns to rows:

@
cols |1 2 3|   |1 4 7|
     |4 5 6| = |2 5 8|
     |7 8 9|   |3 6 9|
@

=== Boxes

@boxs ∘ boxs = id@

That is:

@boxs (boxs a) = a@

Example:

@
cols | 1  2  3  4|   |  1  2 |  5  6 |
     | 5  6  7  8| = |_ 3_ 4_|_ 7_ 8_|
     | 9 10 11 12|   |  9 10 | 13 14 |
     |13 14 15 17|   |_11_12_|_15_17_|
@

=== Group

@ungroup ∘ group = id@

== References

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
              , unknown
              , rows
              , cols
              , boxs
              , group
              , ungroup
              , choices
              , expand
              , valid
              , expand1
              , counts
              , ok
              , complete
              , single
              , search
              , solve
              ) where

import           Data.List (transpose, (\\))

type Grid = Matrix Cell
type Matrix a = [Row a]
type Row a = [a]
type Cell = Char
type Choices = [Cell]

-- | Restriction on 'Cell' data in 'Matrix':
digits :: String
digits = ['1'..'9']

-- | Cell that contain @'0'@ are unknown.
unknown :: Char -> Bool
unknown = (=='0')

-- | Retreive rows (as rows).
rows :: Matrix a -> [Row a]
rows = id

-- | Retreive columns as rows.
cols :: Matrix a -> [Row a]
cols = transpose

-- | Retrieve sudoku boxes from grid as rows.
boxs :: Matrix a -> [Row a]
boxs = map ungroup . ungroup . map cols . group . map group

-- | Group 3 cell boxes from grid.
group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

-- | Ungroup boxes back to cells.
ungroup :: [[a]] -> [a]
ungroup = concat

-- | Choices returns blanks cells filled with
-- all legal values, though they may not be valid.
-- Here 'Cells' with an entry of @'0'@ are unknown.
choices :: Grid -> Matrix Choices
choices = map (map choice)
          where choice v = if unknown v then digits else [v]

-- | Repeat function until value remains unchanged.
-- fix :: Eq a => (a -> a) -> a -> a
-- fix f x = if x == x' then x else fix f x'
--           where x' = f x

-- | Prune invalid cell values from list.
prune :: Matrix Choices -> Matrix Choices
prune = pruneBy boxs . pruneBy cols . pruneBy rows

-- | Reduce choices by pruning redundent cell values.
pruneBy :: ([[Choices]] -> [[Choices]]) -> [[Choices]] -> [[Choices]]
pruneBy f = f . map pruneRow . f

-- | Prune fixed digits from cell choices.
pruneRow :: [Choices] -> [Choices]
pruneRow row = map (remove fixed) row
               where fixed = [d | [d] <- row]

-- | Remove fixed digits from cell of choices.
remove :: Choices -> Choices -> Choices
remove xs ds = if singleton ds then ds else ds \\ xs

-- | Is value in cell a single digit?
singleton :: Foldable t => t a -> Bool
singleton = (1 ==) . length

-- | Expand choices to list of matrices.
expand :: Matrix [a] -> [Matrix a]
expand m = cp (map cp m)

-- | Cartesian product.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [y:ys | y <- xs, ys <- cp xss]

-- | Validate if grid is a valid Sudoku.
valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)

-- | Remove duplicates from cell.
nodups :: Eq a => [a] -> Bool
nodups []     = True
nodups (x:xs) = notElem x xs && nodups xs

-- Sudoku puzzle solver for a easy puzzles.
-- solve' :: Grid -> [Grid]
-- solve' = filter valid . expand . fix prune . choices

-- | Expand the smallest single cell of 'Matrix' of 'Choices'.
expand1 :: Matrix Choices -> [Matrix Choices]
expand1 mc = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
  where
    (rows1, row:rows2) = break (any smallest) mc
    (row1, cs:row2)    = break smallest row
    smallest           = (==n) . length
    n                  = minimum (counts mc)

-- | Break a row in two based on predicate on where to break list.
-- break :: (a -> Bool) -> [a] -> (a, a)
-- break1 p xs = (takeWhile (not p) xs, dropWhile (not p) xs)
-- break = break

-- | Get the length of 'Choices' in a 'Matrix' of 'Choices'.
counts :: Matrix Choices -> [Int]
counts = filter (/=1) . map length . concat

-- | Row is Ok if it contains no duplicates.
ok :: Eq a => [Row a] -> Bool
ok row = nodups [d | [d] <- row]

-- | Matrix is safe if there are no duplicates in 'rows', 'cols' or 'boxs'.
safe :: Eq a => [Matrix a] -> Bool
safe m = all ok (rows m) && all ok (cols m) && all ok (boxs m)

-- | Test it 'Matrix' is complete and safe.
complete :: Matrix Choices -> Bool
complete = all (all single)
--
-- -- | Test for a singleton list.
single :: [a] -> Bool
single = (==1) . length

-- | Search for valid solutions given 'Matrix' of 'Choices'.
search :: Matrix Choices -> [Grid]
search m
  | not (safe m) = []
  | complete m'  = [map (map head) m']
  | otherwise    = concatMap search (expand1 m')
  where m' = prune m

-- | Sudokup puzzle solver.
solve :: Grid -> [Grid]
solve = search . choices

