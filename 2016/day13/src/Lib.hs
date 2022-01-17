module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (guard)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

intToBin :: Int -> [Bool]
intToBin n
  | n <= 1 = [n == 1]
  | otherwise = (n `mod` 2 == 1) : intToBin (n `div` 2)

isSpace :: Int -> Int -> Int -> Bool
isSpace n x y = even . length $ ones
  where
    ones = filter id bin
    bin = intToBin v
    v = x * x + 3 * x + 2 * x * y + y + y * y + n

nextSteps :: Int -> (Int, Int) -> [(Int, Int)]
nextSteps n (x, y) = do
  dx <- [-1 .. 1]
  let x' = x + dx
  guard (x' >= 0)
  dy <- [-1 .. 1]
  let y' = y + dy
  guard (y' >= 0)
  guard ((dx /= 0 || dy /= 0) && (dx == 0 || dy == 0))
  guard (isSpace n x' y')
  return (x', y')

findShortestWayLength :: Int -> Int -> Int -> Int
findShortestWayLength n x y = doer S.empty (S.singleton (1, 1))
  where
    doer visited front
      | (x, y) `S.member` front = 0
      | otherwise = 1 + doer visited' (S.filter (`S.notMember` visited') . S.fromList . concatMap (nextSteps n) . S.toList $ front)
      where
        visited' = visited `S.union` front

part1Solution :: [Int] -> Int
part1Solution [x, y, n, _] = findShortestWayLength n x y

findLocationsOnStep :: Int -> Int -> S.Set (Int, Int)
findLocationsOnStep n finalStep = doer 0 S.empty (S.singleton (1, 1))
  where
    doer step visited front
      | step == finalStep = visited'
      | otherwise = doer (step + 1) visited' (S.filter (`S.notMember` visited') . S.fromList . concatMap (nextSteps n) . S.toList $ front)
      where
        visited' = visited `S.union` front

part2Solution :: [Int] -> Int
part2Solution [_, _, n, steps] = S.size . findLocationsOnStep n $ steps
