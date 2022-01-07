module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

combinations :: Int -> [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n xs
  | n < 0 = []
  | otherwise = concatMap (\(x : xs') -> map (x :) . combinations (n - x) $ xs') . take (length xs) . iterate tail $ xs

part1Solution :: [Int] -> Int
part1Solution = length . combinations 150

part2Solution :: [Int] -> Int
part2Solution cs = length . filter (== mway) $ ways
  where
    ways = map length . combinations 150 $ cs
    mway = minimum ways
