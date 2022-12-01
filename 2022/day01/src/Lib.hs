module Lib
  ( part1Solution,
    part2Solution,
    parseCalories,
  )
where

import Data.List (sortOn)
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

parseCalories :: String -> [[Int]]
parseCalories = map (map read . lines) . splitOn "\n\n"

part1Solution :: [[Int]] -> Int
part1Solution = maximum . map sum

part2Solution :: [[Int]] -> Int
part2Solution = sum . take 3 . sortOn (* (-1)) . map sum
