all: sudoku

sudoku: sudoku.hs
	ghc -o sudoku sudoku.hs