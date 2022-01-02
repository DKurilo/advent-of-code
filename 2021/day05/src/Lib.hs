module Lib
  ( part1Solution,
    part2Solution,
    isHorVer,
    isHorVerDiag,
    Line (..),
  )
where

import Data.Map as M (Map (..), empty, filter, fromList, size, unionWith)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Point = Point Int Int
  deriving (Show, Eq, Ord)

instance Read Point where
  readPrec = do
    x <- readPrec
    (lift . P.char) ','
    Point x <$> readPrec

data Line = Line Point Point

instance Read Line where
  readPrec = do
    p <- readPrec
    (lift . P.string) " -> "
    Line p <$> readPrec

isHor :: Line -> Bool
isHor (Line (Point x1 y1) (Point x2 y2)) = y1 == y2

isVer :: Line -> Bool
isVer (Line (Point x1 y1) (Point x2 y2)) = x1 == x2

isHorVer :: Line -> Bool
isHorVer l = isHor l || isVer l

isDiag :: Line -> Bool
isDiag (Line (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) == abs (y1 - y2)

isHorVerDiag :: Line -> Bool
isHorVerDiag l = isHorVer l || isDiag l

unpackLine :: Line -> Map Point Int
unpackLine l@(Line (Point x1 y1) (Point x2 y2))
  | isVer l = fromList . map (\y -> (Point x1 y, 1)) $ [minY .. maxY]
  | isHor l = fromList . map (\x -> (Point x y1, 1)) $ [minX .. maxX]
  | isDiag l = fromList . map (\i -> (Point (x1 + dx * i) (y1 + dy * i), 1)) $ [0 .. diff]
  | otherwise = empty
  where
    minY = min y1 y2
    maxY = max y1 y2
    minX = min x1 x2
    maxX = max x1 x2
    diff = maxY - minY
    dx = signum (x2 - x1)
    dy = signum (y2 - y1)

part1Solution :: [Line] -> Int
part1Solution = size . M.filter (> 1) . foldl (\m -> unionWith (+) m . unpackLine) empty

part2Solution :: [Line] -> Int
part2Solution = part1Solution
