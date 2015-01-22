import Data.List
import Data.List.Split
import Control.Applicative
import Control.Monad
import System.Environment
import Data.Char
import Data.Maybe
import Data.Ord
import Data.Monoid
import qualified Data.Map as Map

newtype Val = V Int deriving (Eq)
newtype Row = R Int deriving (Eq, Ord, Show)
newtype Col = C Int deriving (Eq, Ord, Show)
type Loc = (Row, Col)
type Status = [Val]
type Board = Map.Map Loc Status

vals = map V [1..9]
rows = map R [0..8]
cols = map C [0..8]
locs = [(r, c) | r <- rows, c <- cols]

emptyBoard :: Board
emptyBoard = Map.fromList . zip locs $ repeat vals

showStatus :: Status -> Char
showStatus []         = 'X'
showStatus [V v] = head $ show v
showStatus _          = '.'

showBoard :: Board -> String
showBoard = unlines . addBlankLines . map formatLine . getLines . statuses
  where addBlankLines = intercalate [""] . chunksOf 3
        formatLine = intersperse ' ' . unwords . chunksOf 3
        getLines = chunksOf 9
        statuses = map showStatus . Map.elems

readStatus :: Char -> Either String Status
readStatus '.' = Right vals
readStatus c   = case index of
    Nothing -> Left $ "Invalid input " ++ [c]
    Just i  -> Right [V $ i + 1]
  where statusChars = ['1'..'9']
        index = elemIndex c statusChars

readBoard :: String -> Either String Board
readBoard cs = if length chars == length locs
    then case mapM readStatus chars of
      Left err -> Left err
      Right ss -> Right . Map.fromList . zip locs $ ss
    else Left "Input must have one character per cell"
  where chars = filter (not . isSpace) cs

sameBox :: Loc -> Loc -> Bool
sameBox (R r1, C c1) (R r2, C c2) =
  (r1 `div` 3 == r2 `div` 3) && (c1 `div` 3 == c2 `div` 3)

sameRow :: Loc -> Loc -> Bool
sameRow (r1, _) (r2, _) = r1 == r2

sameCol :: Loc -> Loc -> Bool
sameCol (_, c1) (_, c2) = c1 == c2

related :: Loc -> Loc -> Bool
related l1 l2 = l1 /= l2 && any check conditions
  where conditions = [sameRow, sameCol, sameBox]
        check c = c l1 l2

relatedLocs :: Loc -> [Loc]
relatedLocs l = filter (related l) locs

eliminate :: Val -> Loc -> Board -> Board
eliminate val loc b =
  if uncertain s && certain s'
    then set b' loc $ head s'
    else b'
  where s = b Map.! loc
        s' = s \\ [val]
        b'  = Map.insert loc s' b

set :: Board -> Loc -> Val -> Board
set b loc v =
  foldr (eliminate v) (Map.insert loc [v] b) $ relatedLocs loc

uncertain :: Status -> Bool
uncertain = (>1) . length

certain :: Status -> Bool
certain = (==1) . length

setGivens :: Board -> Board
setGivens = foldr setGiven emptyBoard . givens
  where givens = filter (certain . snd) . Map.assocs
        setGiven (l, s) b = set b l $ head s

solved :: Board -> Bool
solved = all certain . Map.elems

contradictory :: Board -> Bool
contradictory = any null . Map.elems

guesses :: Board -> [Board]
guesses b = case fu of
    Nothing -> []
    Just (l, s) -> filter (not . contradictory) (set b l <$> s)
  where fu = find (uncertain . snd) $ Map.assocs b

solutions :: Board -> [Board]
solutions b = if solved b
  then [b]
  else guesses b >>= solutions

solve :: Board -> Maybe Board
solve = listToMaybe . solutions

printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "   sudoku <puzzle file>"

printBoard :: Board -> IO ()
printBoard = putStrLn . showBoard

printSolution :: Board -> IO ()
printSolution b = case solution of
    Nothing -> putStrLn "No Solutions"
    Just s  -> printBoard s
  where solution = solve . setGivens $ b

cli :: [String] -> IO ()
cli [f] = do
  input <- readFile f
  let board = readBoard input
  case board of
    Left err -> putStrLn err
    Right b  -> do
      printBoard b
      printSolution b
cli _ = printUsage

main = getArgs >>= cli