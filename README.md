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

```text
-5-|-1-|427
9--|-5-|68-
4--|68-|91-
---+---+---
37-|-62|---
8--|4--|--2
-29|53-|76-
---+---+---
29-|--5|--1
-8-|-2-|-49
---|8-1|--6
```

With a solution of

```text
658|913|427
917|254|683
432|687|915
---+---+---
374|162|598
865|479|132
129|538|764
---+---+---
296|345|871
581|726|349
743|891|256
```

## References

* [Pearls of Functional Algorithm Design](https://dl.acm.org/doi/book/10.5555/1951654)
* [Wikipedia Sudoku](https://en.wikipedia.org/wiki/Sudoku)
* [Sudoku I: First Steps](https://youtu.be/glog9DZh8G0)
* [Sudoku II: Initial Solvers](https://youtu.be/O1-ruHzabAU)
* [Sudoku III: Improving Performance](https://youtu.be/ESDpXBd1cJM)

