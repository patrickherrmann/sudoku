# Sudoku

Build using stack:

```
$ stack install
```

## Usage

To render as ascii instead of unicode, use `sudoku --ascii`.

### Solve

Provide a puzzle file, consisting of 81 characters, with `.`s for blank cells.

```
$ sudoku solve puzzles/extreme.sudoku
╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗
║ 8 │   │   ║   │   │   ║   │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │   │ 3 ║ 6 │   │   ║   │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │ 7 │   ║   │ 9 │   ║ 2 │   │   ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║   │ 5 │   ║   │   │ 7 ║   │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │   │   ║   │ 4 │ 5 ║ 7 │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │   │   ║ 1 │   │   ║   │ 3 │   ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║   │   │ 1 ║   │   │   ║   │ 6 │ 8 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │   │ 8 ║ 5 │   │   ║   │ 1 │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │ 9 │   ║   │   │   ║ 4 │   │   ║
╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝

╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗
║ 8 │ 1 │ 2 ║ 7 │ 5 │ 3 ║ 6 │ 4 │ 9 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 9 │ 4 │ 3 ║ 6 │ 8 │ 2 ║ 1 │ 7 │ 5 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 6 │ 7 │ 5 ║ 4 │ 9 │ 1 ║ 2 │ 8 │ 3 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ 1 │ 5 │ 4 ║ 2 │ 3 │ 7 ║ 8 │ 9 │ 6 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 3 │ 6 │ 9 ║ 8 │ 4 │ 5 ║ 7 │ 2 │ 1 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 2 │ 8 │ 7 ║ 1 │ 6 │ 9 ║ 5 │ 3 │ 4 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ 5 │ 2 │ 1 ║ 9 │ 7 │ 4 ║ 3 │ 6 │ 8 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 4 │ 3 │ 8 ║ 5 │ 2 │ 6 ║ 9 │ 1 │ 7 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 7 │ 9 │ 6 ║ 3 │ 1 │ 8 ║ 4 │ 5 │ 2 ║
╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝

```

To find all possible solutions instead of stopping after one is found, use `solve --all`.

### Generate

Generated puzzles have the following guarantees:
  *  There is exactly one solution
  *  Removing any clue adds additional possible solutions

```
$ sudoku generate
╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗
║ 5 │   │   ║   │ 4 │   ║   │ 9 │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 8 │ 4 │   ║   │   │   ║   │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 6 │   │ 2 ║ 7 │   │   ║   │ 8 │   ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ 1 │   │   ║   │ 5 │   ║ 7 │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │   │   ║ 6 │   │   ║   │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │   │ 9 ║   │   │   ║   │ 4 │ 8 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║   │   │   ║ 9 │   │   ║ 6 │   │ 7 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │ 7 │ 5 ║ 4 │   │   ║ 1 │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │   │   ║   │ 1 │   ║   │   │   ║
╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝

╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗
║ 5 │ 3 │ 7 ║ 8 │ 4 │ 6 ║ 2 │ 9 │ 1 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 8 │ 4 │ 1 ║ 5 │ 9 │ 2 ║ 3 │ 7 │ 6 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 6 │ 9 │ 2 ║ 7 │ 3 │ 1 ║ 4 │ 8 │ 5 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ 1 │ 8 │ 4 ║ 2 │ 5 │ 9 ║ 7 │ 6 │ 3 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 7 │ 5 │ 3 ║ 6 │ 8 │ 4 ║ 9 │ 1 │ 2 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 2 │ 6 │ 9 ║ 1 │ 7 │ 3 ║ 5 │ 4 │ 8 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ 4 │ 1 │ 8 ║ 9 │ 2 │ 5 ║ 6 │ 3 │ 7 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 3 │ 7 │ 5 ║ 4 │ 6 │ 8 ║ 1 │ 2 │ 9 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 9 │ 2 │ 6 ║ 3 │ 1 │ 7 ║ 8 │ 5 │ 4 ║
╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝

```

To generate a puzzle without showing the solution, use `generate --hideSolution`.