module Lib (part1Solution) where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

-- magic sequence from task
codes :: Int -> Int
codes 1 = 20151125
codes n = (codes (n - 1) * 252533) `mod` 33554393

part1Solution :: Int -> Int -> Int
part1Solution x y = codes n
  where
    -- number of numbers in diagonal is diagonal number
    -- diagonal number is x + y - 1
    -- so in some cell (x, y) we have:
    n = let xy = x + y - 1 in ((xy - 1) * xy) `div` 2 + x
