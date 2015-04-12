import CLI
import Sudoku
import Options.Applicative

printSolution :: Board -> IO ()
printSolution b = case solve b of
  Nothing -> putStrLn "No solutions"
  Just s  -> putStrLn $ showBoard s

printSolutions :: Board -> IO ()
printSolutions b = case solutions b of
  [] -> putStrLn "No solutions"
  ss -> mapM_ (putStrLn . showBoard) ss

solveCommand :: SolveOptions -> IO ()
solveCommand opts = do
  f <- readFile $ puzzleFile opts
  case readBoard f of
    Left err -> putStrLn err
    Right b  -> do
      putStrLn $ showBoard b
      if allSolutions opts
        then printSolutions b
        else printSolution b

main :: IO ()
main = do
  opts <- execParser optParser
  case cmd opts of
    Solve solveOpts -> solveCommand solveOpts