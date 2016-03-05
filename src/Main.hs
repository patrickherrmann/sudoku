import CLI
import Sudoku
import Control.Monad
import Data.Random

main :: IO ()
main = do
  opts <- parseOpts
  case cmd opts of
    Solve sOpts -> solveCommand opts sOpts
    Generate gOpts -> generateCommand opts gOpts

solveCommand :: Options -> SolveOptions -> IO ()
solveCommand opts sOpts = do
  f <- readFile $ puzzleFile sOpts
  case readBoard f of
    Left err -> putStrLn err
    Right b  -> do
      putStrLn $ showBoard opts b
      if allSolutions sOpts
        then printSolutions opts b
        else printSolution opts b

generateCommand :: Options -> GenerateOptions -> IO ()
generateCommand opts gOpts = do
  (puzzle, solution) <- sample randomPuzzle
  putStrLn $ showBoard opts puzzle
  unless (hideSolution gOpts) (putStrLn $ showBoard opts solution)

showBoard :: Options -> Board -> String
showBoard opts
  | useAscii opts = showBoardAscii
  | otherwise = showBoardUnicode

printSolution :: Options -> Board -> IO ()
printSolution opts b = case solve b of
  Nothing -> putStrLn "No solutions"
  Just s  -> putStrLn $ showBoard opts s

printSolutions :: Options -> Board -> IO ()
printSolutions opts b = case solutions b of
  [] -> putStrLn "No solutions"
  ss -> mapM_ (putStrLn . showBoard opts) ss
