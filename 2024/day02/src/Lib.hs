module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List.Split (splitOn)

parseLine :: String -> [Int]
parseLine = fmap read . splitOn " "

isSafe :: [Int] -> Bool
isSafe xs = all p xxs && (all (>0) xxs || all (<0) xxs)
    where
        p x = let x' = abs x in x' >= 1 && x' <= 3
        xxs = zipWith (-) xs . tail $ xs

part1Solution :: [String] -> Int
part1Solution = length . filter isSafe . fmap parseLine

isSafeOneErr :: [Int] -> Bool
isSafeOneErr xs = any isSafe (xs : [take i xs <> drop (i + 1) xs | i <- [0..length xs - 1]])

part2Solution :: [String] -> Int
part2Solution = length . filter isSafeOneErr . fmap parseLine
