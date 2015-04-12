# Sudoku

Build using cabal:

```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
```

## Usage

```
$ ./dist/build/sudoku/sudoku solve puzzles/hardest.sudoku
┏━━━┯━━━┯━━━┳━━━┯━━━┯━━━┳━━━┯━━━┯━━━┓
┃ 8 │   │   ┃   │   │   ┃   │   │   ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃   │   │ 3 ┃ 6 │   │   ┃   │   │   ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃   │ 7 │   ┃   │ 9 │   ┃ 2 │   │   ┃
┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫
┃   │ 5 │   ┃   │   │ 7 ┃   │   │   ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃   │   │   ┃   │ 4 │ 5 ┃ 7 │   │   ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃   │   │   ┃ 1 │   │   ┃   │ 3 │   ┃
┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫
┃   │   │ 1 ┃   │   │   ┃   │ 6 │ 8 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃   │   │ 8 ┃ 5 │   │   ┃   │ 1 │   ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃   │ 9 │   ┃   │   │   ┃ 4 │   │   ┃
┗━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┛

┏━━━┯━━━┯━━━┳━━━┯━━━┯━━━┳━━━┯━━━┯━━━┓
┃ 8 │ 1 │ 2 ┃ 7 │ 5 │ 3 ┃ 6 │ 4 │ 9 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 9 │ 4 │ 3 ┃ 6 │ 8 │ 2 ┃ 1 │ 7 │ 5 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 6 │ 7 │ 5 ┃ 4 │ 9 │ 1 ┃ 2 │ 8 │ 3 ┃
┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫
┃ 1 │ 5 │ 4 ┃ 2 │ 3 │ 7 ┃ 8 │ 9 │ 6 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 3 │ 6 │ 9 ┃ 8 │ 4 │ 5 ┃ 7 │ 2 │ 1 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 2 │ 8 │ 7 ┃ 1 │ 6 │ 9 ┃ 5 │ 3 │ 4 ┃
┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫
┃ 5 │ 2 │ 1 ┃ 9 │ 7 │ 4 ┃ 3 │ 6 │ 8 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 4 │ 3 │ 8 ┃ 5 │ 2 │ 6 ┃ 9 │ 1 │ 7 ┃
┠───┼───┼───╂───┼───┼───╂───┼───┼───┨
┃ 7 │ 9 │ 6 ┃ 3 │ 1 │ 8 ┃ 4 │ 5 │ 2 ┃
┗━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┛

```

To render as ascii instead of unicode, use `sudoku --ascii`. To find all possible solutions instead of stopping after one is found, use `solve --all`.