# Sudoku Solver

This is an implementation of Richard Bird's Sudoku Solver from
[Pearls of Functional Algorithm Design](https://dl.acm.org/doi/book/10.5555/1951654).

## Build

To build this application, run:

```bash
make clean setup check build
```

## Documentation

API documentation is available from GitLab pages,
[here](https://frankhjung1.gitlab.io/haskell-sudoku/).

## Example

Run example Sudoku solver against puzzle read from a file:

```bash
cabal exec sudoku -- data/easy.sudoku
```

This is an easy Sudoku puzzle:

![Easy Puzzle](data/easy-puzzle.png)

With a solution of

![Easy Solution](data/easy-solution.png)

## References

* [Pearls of Functional Algorithm Design](https://dl.acm.org/doi/book/10.5555/1951654)
* [Wikipedia Sudoku](https://en.wikipedia.org/wiki/Sudoku)
* [Sudoku I: First Steps](https://youtu.be/glog9DZh8G0)
* [Sudoku II: Initial Solvers](https://youtu.be/O1-ruHzabAU)
* [Sudoku III: Improving Performance](https://youtu.be/ESDpXBd1cJM)

