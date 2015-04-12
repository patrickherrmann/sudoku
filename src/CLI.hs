module CLI
  ( Options(..)
  , Command(..)
  , SolveOptions(..)
  , optParser
  ) where

import Options.Applicative

data Options = Options
  { cmd :: Command
  }

data Command
  = Solve SolveOptions

data SolveOptions = SolveOptions
  { puzzleFile :: String
  , allSolutions :: Bool
  }

parsePuzzleFile = argument str $ metavar "PUZZLE_FILE"

parseAllSolutions = flag False True
  $  long "all"
  <> short 'a'
  <> help "Find all solutions instead of stopping at one"

parseSolveOptions = SolveOptions
  <$> parsePuzzleFile
  <*> parseAllSolutions

parseSolveOptionInfo = info (helper <*> parseSolveOptions)
  $  progDesc "Solve a sudoku puzzle in PUZZLE_FILE"

parseCommand = subparser
  $  command "solve" (Solve <$> parseSolveOptionInfo)

parseOptions = Options
  <$> parseCommand

optParser = info (helper <*> parseOptions)
           $  fullDesc
           <> header "Solve sudoku puzzles"
           <> progDesc "Solve sudoku puzzles"