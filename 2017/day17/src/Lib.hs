module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (dropWhile, elemIndex)
import Debug.Trace (trace)

spin :: [Int] -> Int -> Int -> Int -> [[Int]]
spin xs pos x step = xs : spin xs' pos' (x + 1) step
  where
    pos' = 1 + (pos + step) `mod` length xs
    xs' = take pos' xs ++ ((x + 1) : drop pos' xs)

part1Solution :: Int -> Int
part1Solution = (!! 1) . dropWhile (/= 2017) . (!! 2017) . spin [0] 0 0

rapidSpin :: Int -> Int -> Int
rapidSpin spins step = doer 0 0 0
  where
    doer n result pos
      | n >= spins = result
      | pos' == 1 = doer (n + 1) (n + 1) pos'
      | otherwise = doer (n + 1) result pos'
      where
        pos' = 1 + ((pos + step) `mod` (n + 1))

part2Solution :: Int -> Int
part2Solution = rapidSpin 50000000
