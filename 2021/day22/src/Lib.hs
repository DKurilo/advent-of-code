module Lib
  ( part1Solution,
    part2Solution,
    Rule (..),
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Interval = Interval Int Int deriving (Show)

instance Read Interval where
  readPrec = do
    x <- readPrec
    (lift . P.string) ".."
    Interval x <$> readPrec

intervalLength (Interval from to) = to - from + 1

overlappingIntervals :: Interval -> Interval -> Bool
overlappingIntervals (Interval x1 x2) (Interval y1 y2)
  | x1 >= y1 && x1 <= y2 = True
  | x2 >= y1 && x2 <= y2 = True
  | y1 >= x1 && y1 <= x2 = True
  | y2 >= x1 && y2 <= x2 = True
  | otherwise = False

subIntervals :: Interval -> Interval -> [Interval]
subIntervals ix@(Interval x1 x2) iy@(Interval y1 y2)
  | not (overlappingIntervals ix iy) = [ix]
  | y1 <= x1 && y2 >= x2 = []
  | y1 > x1 && y2 < x2 = [Interval x1 (y1 - 1), Interval (y2 + 1) x2]
  | y1 <= x1 && y2 < x2 = [Interval (y2 + 1) x2]
  | y1 > x1 && y2 >= x2 = [Interval x1 (y1 - 1)]

subPlanes :: (Interval, Interval) -> (Interval, Interval) -> [(Interval, Interval)]
subPlanes (ix1, iy1) (ix2, iy2)
  | not (overlappingIntervals ix1 ix2) && not (overlappingIntervals iy1 iy2) = [(ix1, iy1)]
  | otherwise = [(ix', iy1) | ix' <- ixs] ++ [(ix, iy) | iy <- iys]
  where
    ixs = subIntervals ix1 ix2
    ix = foldl (\i1 i2 -> head (subIntervals i1 i2)) ix1 ixs
    iys = subIntervals iy1 iy2

data Rule = On Interval Interval Interval | Off Interval Interval Interval | Filter Interval Interval Interval deriving (Show)

instance Read Rule where
  readPrec = do
    rule <- (lift . P.string) "on" PC.+++ (lift . P.string) "off"
    (lift . P.string) " x="
    ix <- readPrec
    (lift . P.string) ",y="
    iy <- readPrec
    (lift . P.string) ",z="
    ( case rule of
        "on" -> On
        _ -> Off
      )
      ix
      iy
      <$> readPrec

data Cuboid = Cuboid Interval Interval Interval deriving (Show)

overlappingCuboids :: Cuboid -> Cuboid -> Bool
overlappingCuboids (Cuboid ix1 iy1 iz1) (Cuboid ix2 iy2 iz2) =
  overlappingIntervals ix1 ix2
    && overlappingIntervals iy1 iy2
    && overlappingIntervals iz1 iz2

subCuboids :: Cuboid -> Cuboid -> [Cuboid]
subCuboids c1@(Cuboid ix1 iy1 iz1) c2@(Cuboid ix2 iy2 iz2)
  | not (overlappingCuboids c1 c2) = [c1]
  | otherwise = [Cuboid ix' iy' iz1 | (ix', iy') <- pxys] ++ [Cuboid ix iy iz | iz <- izs]
  where
    pxys = subPlanes (ix1, iy1) (ix2, iy2)
    (ix, iy) = foldl (\p1 p2 -> head (subPlanes p1 p2)) (ix1, iy1) pxys
    izs = subIntervals iz1 iz2

volume :: Cuboid -> Int
volume (Cuboid ix iy iz) = intervalLength ix * intervalLength iy * intervalLength iz

type Reactor = [Cuboid]

applyRule :: Rule -> Reactor -> Reactor
applyRule (Filter ix iy iz) = inv . inv
  where
    inv = foldl (\r' (Cuboid ix' iy' iz') -> applyRule (Off ix' iy' iz') r') [Cuboid ix iy iz]
applyRule (Off ix iy iz) = concatMap (`subCuboids` Cuboid ix iy iz)
applyRule (On ix iy iz) = doer [Cuboid ix iy iz]
  where
    doer :: [Cuboid] -> Reactor -> Reactor
    doer cs [] = cs
    doer cs (c' : cs') = c' : doer (concatMap (`subCuboids` c') cs) cs'

count :: Reactor -> Int
count = sum . map volume

part1Solution :: [Rule] -> Int
part1Solution =
  count
    . applyRule (Filter (Interval (-50) 50) (Interval (-50) 50) (Interval (-50) 50))
    . foldl (flip applyRule) []

part2Solution :: [Rule] -> Int
part2Solution = count . foldl (flip applyRule) []
