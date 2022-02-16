module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

fuel :: Int -> Int
fuel n = n `div` 3 - 2

fuelfuel :: Int -> Int
fuelfuel n
  | n' < 0 = 0
  | otherwise = n' + fuelfuel n'
  where
    n' = fuel n

part1Solution :: [Int] -> Int
part1Solution = sum . map fuel

part2Solution :: [Int] -> Int
part2Solution = sum . map fuelfuel
