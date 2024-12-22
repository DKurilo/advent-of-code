module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.Bits (Bits(shift, xor))
import Data.List (zipWith5)
import qualified Data.Map as M

modulo :: Int
modulo = 16777216

op1Shift :: Int
op1Shift = 6

op2Shift :: Int
op2Shift = -5

op3Shift :: Int
op3Shift = 11

op :: Int -> Int -> Int -> Int
op sh m x = (x `xor` (x `shift` sh)) `mod` m

nextRand :: Int -> Int
nextRand = op op3Shift modulo . op op2Shift modulo . op op1Shift modulo

part1Solution :: [Int] -> Int
part1Solution = sum . fmap ((!! 2000) . iterate nextRand)

price :: Int -> Int
price = (`mod` 10)

sequences :: Int -> Int -> M.Map (Int, Int, Int, Int) Int
sequences n x0 =
  M.fromList . reverse
    $ zipWith5
        (\p1 p2 p3 p4 p5 -> ((p2 - p1, p3 - p2, p4 - p3, p5 - p4), p5))
        prices
        (tail prices)
        (drop 2 prices)
        (drop 3 prices)
        (drop 4 prices)
  where
    prices = fmap price . take n . iterate nextRand $ x0

part2Solution :: [Int] -> Int
part2Solution = maximum . M.elems . M.unionsWith (+) . fmap (sequences 2000)
