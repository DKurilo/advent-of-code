module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (bimap)
import Data.List (foldl', sort)
import Data.Char (isDigit)

parseLine :: String -> (Int, Int)
parseLine cs = (read sl, read sr)
    where
        sl = takeWhile isDigit cs
        sr = dropWhile (== ' ') . dropWhile isDigit $ cs

parse :: [String] -> ([Int], [Int])
parse = bimap reverse reverse
      . foldl' (\(leftNs, rightNs) cs -> let (l, r) = parseLine cs in (l: leftNs, r: rightNs)) ([], [])

part1Solution :: [String] -> Int
part1Solution ls = sum (zipWith (\n m -> abs (n - m)) sns sms)
    where
        (ns, ms) = parse ls
        sns = sort ns
        sms = sort ms


part2Solution :: [String] -> Int
part2Solution ls = sum . fmap (\n -> n * (length . filter (== n)) ms ) $ ns
    where
        (ns, ms) = parse ls
