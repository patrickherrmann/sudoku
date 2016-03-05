module Sudoku
  ( Val(..)
  , Row(..)
  , Col(..)
  , Loc
  , Status
  , Board
  , solutions
  , solve
  , randomPuzzle
  , readBoard
  , showBoardAscii
  , showBoardUnicode
  ) where

import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import Data.Random
import Data.Ord
import Data.Array.IArray

newtype Val = V Int deriving (Eq)
newtype Row = R Int deriving (Eq, Ord, Show, Ix)
newtype Col = C Int deriving (Eq, Ord, Show, Ix)
type Loc = (Row, Col)
type Status = [Val]
type Board = Array Loc Status

vals :: [Val]
vals = map V [1..9]

rows :: [Row]
rows = map R [0..8]

cols :: [Col]
cols = map C [0..8]

locs :: [Loc]
locs = [(r, c) | r <- rows, c <- cols]

boardBounds :: (Loc, Loc)
boardBounds = ((R 0, C 0), (R 8, C 8))

emptyBoard :: Board
emptyBoard = listArray boardBounds $ repeat vals

sameBox :: Loc -> Loc -> Bool
sameBox (R r1, C c1) (R r2, C c2) =
  (r1 `div` 3 == r2 `div` 3) && (c1 `div` 3 == c2 `div` 3)

sameRow :: Loc -> Loc -> Bool
sameRow (r1, _) (r2, _) = r1 == r2

sameCol :: Loc -> Loc -> Bool
sameCol (_, c1) (_, c2) = c1 == c2

related :: Loc -> Loc -> Bool
related l1 l2 = l1 /= l2 && any check conditions
  where
    conditions = [sameRow, sameCol, sameBox]
    check c = c l1 l2

relatedLocs :: Loc -> [Loc]
relatedLocs l = filter (related l) locs

eliminate :: Val -> Loc -> Board -> Board
eliminate v loc b
    | null isV = b
    | otherwise = reduce notV
  where
    (isV, notV) = partition (== v) $ b ! loc
    reduce [v'] = set b loc v'
    reduce _    = b // [(loc, notV)]

set :: Board -> Loc -> Val -> Board
set b loc v = foldr (eliminate v) (b // [(loc, [v])]) $ relatedLocs loc

uncertain :: Status -> Bool
uncertain = \case
  _:_:_ -> True
  _ -> False

certain :: Status -> Bool
certain = \case
  [_] -> True
  _ -> False

setGivens :: Board -> Board
setGivens = foldr setGiven emptyBoard . givens
  where
    givens = mapMaybe given . assocs
    given (l, [v]) = Just (l, v)
    given _ = Nothing
    setGiven (l, v) b = set b l v

solved :: Board -> Bool
solved = all certain

contradictory :: Board -> Bool
contradictory = any null . elems

bestGuesses :: Board -> [(Loc, Status)]
bestGuesses = sortBy (comparing (length <$>))
            . filter (uncertain . snd)
            . assocs

guesses :: Board -> [Board]
guesses b = set b l <$> s
  where ((l, s):_) = bestGuesses b

findSolutions :: Board -> [Board]
findSolutions b
  | contradictory b = []
  | solved b = [b]
  | otherwise = guesses b >>= findSolutions

solutions :: Board -> [Board]
solutions = findSolutions . setGivens

solve :: Board -> Maybe Board
solve = listToMaybe . solutions

rotateBoard :: Board -> Board
rotateBoard = ixmap boardBounds rotateLoc
  where rotateLoc (R r, C c) = (R c, C (8 - r))

shuffleColumns :: Board -> RVar Board
shuffleColumns b = do
  let ts = chunksOf 3 cols
  ts' <- concat <$> (mapM shuffle ts >>= shuffle)
  let transform (C c) = ts' !! c
  return $ ixmap boardBounds (transform <$>) b

shuffleSymbols :: Board -> RVar Board
shuffleSymbols b = do
  vs <- shuffle vals
  let transform (V v) = vs !! (v - 1)
  return $ amap (transform <$>) b

shuffleStatuses :: Board -> RVar Board
shuffleStatuses b = do
  ss <- mapM shuffle $ elems b
  return $ listArray boardBounds ss

randomBoard :: RVar Board
randomBoard = do
  shuffleStatuses emptyBoard
    >>= return . head . findSolutions
    >>= shuffleColumns
    >>= return . rotateBoard
    >>= shuffleColumns
    >>= shuffleSymbols

removeGivens :: Board -> [Board]
removeGivens b = do
  loc <- map fst . filter (certain . snd) $ assocs b
  return $ b // [(loc, vals)]

ambiguous :: Board -> Bool
ambiguous = (>1) . length . take 2 . solutions

ambiguate :: Board -> RVar Board
ambiguate b = case filter (not . ambiguous) $ removeGivens b of
  [] -> return b
  bs -> randomElement bs >>= ambiguate

randomPuzzle :: RVar (Board, Board)
randomPuzzle = do
  rb <- randomBoard
  rp <- ambiguate rb
  return (rp, rb)

showStatusAscii :: Status -> Char
showStatusAscii = \case
  [] -> 'X'
  [V v] -> head $ show v
  _ -> '.'

showStatusUnicode :: Status -> Char
showStatusUnicode = \case
  [] -> 'X'
  [V v] -> head $ show v
  _ -> ' '

showBoardAscii :: Board -> String
showBoardAscii = unlines . addBlankLines . map formatLine . chunksOf 9 . statuses
  where
    addBlankLines = intercalate [""] . chunksOf 3
    formatLine = intersperse ' ' . unwords . chunksOf 3
    statuses = map showStatusAscii . elems

showBoardUnicode :: Board -> String
showBoardUnicode = addCaps . unlines . addDividers . map formatLine . chunksOf 9 . cells
  where
    addDividers = intercalate [bigDivider] . map (intersperse smallDivider) . chunksOf 3
    bigDivider = "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣"
    smallDivider = "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢"
    addCaps = (topRow ++) . (++ bottomRow)
    topRow = "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗\n"
    bottomRow = "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝\n"
    formatLine = ('║' :) . (++ "║") . intercalate "║" . map formatChunk . chunksOf 3
    formatChunk = intercalate "│"
    padCell s = [' ', s, ' ']
    cells = map (padCell . showStatusUnicode) . elems

readStatus :: Char -> Either String Status
readStatus '.' = Right vals
readStatus c   = case mi of
    Nothing -> Left $ "Invalid input " ++ [c]
    Just i  -> Right [V $ i + 1]
  where
    statusChars = ['1'..'9']
    mi = elemIndex c statusChars

readBoard :: String -> Either String Board
readBoard cs = if length chars == length locs
    then case mapM readStatus chars of
      Left err -> Left err
      Right ss -> Right . listArray boardBounds $ ss
    else Left "Input must have one character per cell"
  where chars = filter (not . isSpace) cs