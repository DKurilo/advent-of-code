module Lib (
    part1Solution,
    part2Solution,
)
where

import Debug.Trace (trace)

parsePowerBank :: String -> [Int]
parsePowerBank = fmap (read . (: []))

maxPower :: Int -> [Int] -> Int
maxPower 0 _ = 0
maxPower n xs = (10 ^ (n - 1)) * max1 + maxPower (n - 1) (tail . dropWhile (/= max1) $ xs)
  where
    lxs = length xs
    max1 = maximum . take (lxs - n + 1) $ xs

part1Solution :: [String] -> Int
part1Solution = sum . fmap (maxPower 2 . parsePowerBank)

part2Solution :: [String] -> Int
part2Solution = sum . fmap (maxPower 12 . parsePowerBank)
