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
uncertain s = length s > 1

certain :: Status -> Bool
certain s = length s == 1

setGivens :: Board -> Board
setGivens board = foldr (\(l, s) -> \b -> set b l $ head s) emptyBoard givens
  where givens = filter (certain . snd) $ Map.assocs board

solved :: Board -> Bool
solved = all ((==1) . length) . Map.elems

contradictory :: Board -> Bool
contradictory = any null . Map.elems

guesses :: Board -> [Board]
guesses b = case fu of
              Nothing -> []
              Just (l, s) -> filter (not . contradictory) (set b l <$> s)
  where fu = find ((>1) . length . snd) $ Map.assocs b


solutions :: Board -> [Board]
solutions b = if solved b
                then [b]
                else guesses b >>= solutions

solve :: Board -> Maybe Board
solve = listToMaybe . solutions

main = do
   args <- getArgs
   input <- readFile $ head args
   let board = readBoard input
   putStrLn . showBoard $ board
   let solution = solve . setGivens $ board
   case solution of
      Nothing -> putStrLn "No Solutions"
      Just s  -> putStrLn . showBoard $ s