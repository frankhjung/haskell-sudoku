# Sudoku Solver

This is a Haskell implementation of Richard Bird's Sudoku Solver from
[Pearls of Functional Algorithm Design](https://dl.acm.org/doi/book/10.5555/1951654).

The solver operates primarily at the row level. To process columns and 3×3
internal boxes, the algorithm transforms them into rows, processes them, and
then restores the original structure. This approach relies on the reversible
nature of these transformations (see Properties).

The algorithm performs the following steps on each row (including transformed
columns and boxes):

* Fills unknown cells with all available digits.
* Removes fixed digits from these choices.
* Identifies the first cell with the minimum number of choices (greater than
  one).
* Expands that single cell into matching choice matrices.
* Recursively searches each expanded 'Matrix' for the next minimum cell to prune
  and expand.
* Terminates when it finds either an invalid 'Matrix' or a solution 'Matrix'
  containing only 'singleton' cells.

## Implementation Details

The codebase includes helper functions for specific Sudoku properties:

* `safe`: Verifies a matrix is safe (no duplicates in rows, columns, or boxes).
* `complete`: Checks if a matrix is complete (contains only singleton cells).
* `choices`: Fills unknown cells with candidate digits.
* `expand1`: Expands the cell with the fewest choices.
* `search`: The main recursive solver.

The implementation relies on properties of `rows`, `cols`, `boxs`, `group`, and
`ungroup`. For example, `rows ∘ rows = id` implies `rows (rows a) = a`.
Similarly, `cols ∘ cols = id`, `boxs ∘ boxs = id`, and `ungroup ∘ group = id`.

## Build

To build this application, run:

```bash
make
```

This will run the `default` goal of format, check, build and test.

## Documentation

API documentation is available from GitLab pages,
[frankhjung1.gitlab.io/haskell-sudoku](https://frankhjung1.gitlab.io/haskell-sudoku/).

## Example

Run example Sudoku solver against puzzle read from a file:

```bash
stack exec sudoku -- data/easy.sudoku
```

This is an easy Sudoku puzzle:

![Easy Puzzle](data/easy-puzzle.png)

With a solution of

![Easy Solution](data/easy-solution.png)

## References

* [Pearls of Functional Algorithm Design](https://dl.acm.org/doi/book/10.5555/1951654)
* [Wikipedia Sudoku](https://en.wikipedia.org/wiki/Sudoku)
