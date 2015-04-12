import CLI
import Sudoku
import Options.Applicative

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

main :: IO ()
main = do
  opts <- execParser optParser
  case cmd opts of
    Solve solveOpts -> solveCommand opts solveOpts