module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (guard)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

checksum :: [Int] -> Int
checksum ns = maximum ns - minimum ns

evenlyDivisible :: [Int] -> Int
evenlyDivisible xs = head $ do
  i <- [0 .. length xs - 1]
  j <- [0 .. length xs - 1]
  guard $ i /= j
  let m = xs !! i
      n = xs !! j
  guard $ m >= n
  guard $ m `mod` n == 0
  return (m `div` n)

part1Solution :: [[Int]] -> Int
part1Solution = sum . map checksum

part2Solution :: [[Int]] -> Int
part2Solution = sum . map evenlyDivisible
