module CLI
  ( Options(..)
  , Command(..)
  , SolveOptions(..)
  , GenerateOptions(..)
  , parseOpts
  ) where

import Options.Applicative

data Options = Options
  { cmd :: Command
  , useAscii :: Bool
  }

data Command
  = Solve SolveOptions
  | Generate GenerateOptions

data SolveOptions = SolveOptions
  { puzzleFile :: String
  , allSolutions :: Bool
  }

data GenerateOptions = GenerateOptions
  { hideSolution :: Bool
  }

parseOpts :: IO Options
parseOpts = customExecParser (prefs showHelpOnError) optParser
  where
    optParser = info (helper <*> parseOptions)
               $  fullDesc
               <> progDesc "Solve and generate sudoku puzzles"

    parseOptions = Options
      <$> parseCommand
      <*> parseUseAscii

    parseCommand = subparser
      $  command "solve" (Solve <$> parseSolveOptionInfo)
      <> command "generate" (Generate <$> parseGenerateOptionInfo)
    
    parseUseAscii = flag False True
      $  long "ascii"
      <> help "Render the board in ascii instead of unicode"

    parseSolveOptionInfo = info (helper <*> parseSolveOptions)
      $  progDesc "Solve a sudoku puzzle in PUZZLE_FILE"

    parseSolveOptions = SolveOptions
      <$> parsePuzzleFile
      <*> parseAllSolutions

    parseGenerateOptionInfo = info (helper <*> parseGenerateOptions)
      $  progDesc "Generate a random sudoku puzzle"
      
    parseGenerateOptions = GenerateOptions
      <$> parseHideSolution
      
    parsePuzzleFile = argument str $ metavar "PUZZLE_FILE"

    parseAllSolutions = flag False True
      $  long "all"
      <> short 'a'
      <> help "Find all solutions instead of stopping at one"

    parseHideSolution = flag False True
      $  long "hideSolution"
      <> help "Only show the puzzle, not its solution"
