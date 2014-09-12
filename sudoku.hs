import Data.List
import Data.List.Split
import Control.Applicative
import Control.Monad
import System.Environment
import Data.Char
import qualified Data.Map as Map

newtype Val = V Int deriving (Eq)
newtype Row = R Int deriving (Eq, Ord)
newtype Col = C Int deriving (Eq, Ord)
type Loc = (Row, Col)
type Status = [Val]
type Cell = (Loc, Status)
type Board = Map.Map Loc Status

vals = map V [1..9]
rows = map R [0..8]
cols = map C [0..8]
locs = [(r, c) | r <- rows, c <- cols]

emptyBoard :: Board
emptyBoard = Map.fromList . zip locs $ repeat vals

showStatus :: Status -> Char
showStatus []         = 'X'
showStatus ((V v):[]) = head $ show v
showStatus _          = '.'

showBoard :: Board -> String
showBoard = unlines
          . concat
          . intersperse [""]
          . chunksOf 3
          . (map $ intersperse ' ' . unwords . chunksOf 3)
          . chunksOf 9
          . map showStatus
          . Map.elems

readStatus :: Char -> Status
readStatus '.' = vals
readStatus 'X' = []
readStatus v   = [V (read [v] :: Int)]

readBoard :: String -> Board
readBoard = Map.fromList . zip locs . map readStatus . filter (not . isSpace)

sameBox :: Loc -> Loc -> Bool
sameBox (R r1, C c1) (R r2, C c2) =
  (r1 `div` 3 == r2 `div` 3) && (c1 `div` 3 == c2 `div` 3)

sameRow :: Loc -> Loc -> Bool
sameRow (r1, _) (r2, _) = r1 == r2

sameCol :: Loc -> Loc -> Bool
sameCol (_, c1) (_, c2) = c1 == c2

related :: Loc -> Loc -> Bool
related l1 l2 = l1 /= l2 && or ([sameRow, sameCol, sameBox] <*> [l1] <*> [l2])

relatedLocs :: Loc -> [Loc]
relatedLocs l = filter (related l) locs

eliminate :: Val -> Loc -> Board -> Board
eliminate val loc b =
  if length old == 2 && length new == 1
    then markAsCertain (head new) loc b'
    else b'
  where old = b Map.! loc
        new = old \\ [val]
        b'  = Map.insert loc new b

markAsCertain :: Val -> Loc -> Board -> Board
markAsCertain v loc b =
  foldr (eliminate v) (Map.insert loc [v] b) $ relatedLocs loc

markGivenCertainties :: Board -> Board
markGivenCertainties b =
  foldr (\(loc, status) -> markAsCertain (head status) loc) emptyBoard givens
  where givens = filter (\(loc, status) -> length status == 1) $ Map.assocs b

solved :: Board -> Bool
solved = all (==1) . map length . Map.elems

contradictory :: Board -> Bool
contradictory = any null . Map.elems

possibilities :: Board -> [Board]
possibilities b = concat $ map (possibleChanges b) (Map.assocs b)

possibleChanges :: Board -> Cell -> [Board]
possibleChanges b (loc, status) =
  if length status > 1
    then filter (not . contradictory) $
         markAsCertain <$> status <*> [loc] <*> [b]
    else []

findSolution :: [Board] -> Maybe Board
findSolution [] = Nothing
findSolution bs =
  case find solved bs of
    Just s  -> Just s
    Nothing -> findSolution $ bs >>= possibilities

solve :: Board -> Maybe Board
solve b = findSolution [markGivenCertainties b]

main = do
   args <- getArgs
   input <- readFile $ head args
   let board = readBoard input
   let output = case solve board of
                  Just solution -> showBoard solution
                  Nothing       -> "Unsolvable"
   putStr output