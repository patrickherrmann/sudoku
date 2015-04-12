module Sudoku
  ( Val(..)
  , Row(..)
  , Col(..)
  , Loc
  , Status
  , Board
  , readBoard
  , showBoard
  , showBoardUnicode
  , solutions
  , solve
  ) where

import Data.List
import Data.List.Split
import Control.Applicative
import Data.Char
import Data.Maybe
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

showStatusUnicode :: Status -> Char
showStatusUnicode []         = 'X'
showStatusUnicode [V v] = head $ show v
showStatusUnicode _          = ' '

showBoard :: Board -> String
showBoard = unlines . addBlankLines . map formatLine . chunksOf 9 . statuses
  where addBlankLines = intercalate [""] . chunksOf 3
        formatLine = intersperse ' ' . unwords . chunksOf 3
        statuses = map showStatus . Map.elems

showBoardUnicode :: Board -> String
showBoardUnicode = addCaps . unlines . addDividers . map formatLine . chunksOf 9 . cells
  where addDividers = intercalate [bigDivider] . map (intersperse smallDivider) . chunksOf 3
        bigDivider = "┣━━━┿━━━┿━━━╋━━━┿━━━┿━━━╋━━━┿━━━┿━━━┫"
        smallDivider = "┠───┼───┼───╂───┼───┼───╂───┼───┼───┨"
        addCaps = (topRow ++) . (++ bottomRow)
        topRow = "┏━━━┯━━━┯━━━┳━━━┯━━━┯━━━┳━━━┯━━━┯━━━┓\n"
        bottomRow = "┗━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┷━━━┛\n"
        formatLine = ('┃' :) . (++ "┃") . intercalate "┃" . map formatChunk . chunksOf 3
        formatChunk = intercalate "│"
        padCell s = [' ', s, ' ']
        cells = map (padCell . showStatusUnicode) . Map.elems

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
        setGiven (l, [v]) b = set b l v

solved :: Board -> Bool
solved = all certain . Map.elems

contradictory :: Board -> Bool
contradictory = any null . Map.elems

bestGuess :: Board -> Maybe (Loc, Status)
bestGuess b = listToMaybe candidates
  where candidates = filter (uncertain . snd)
                   . Map.assocs $ b

guesses :: Board -> [Board]
guesses b = case bestGuess b of
    Nothing -> []
    Just (l, s) -> filter (not . contradictory) (set b l <$> s)

solutions' :: Board -> [Board]
solutions' b = if solved b
  then [b]
  else guesses b >>= solutions

solutions :: Board -> [Board]
solutions = solutions' . setGivens

solve :: Board -> Maybe Board
solve = listToMaybe . solutions