module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Maybe (fromMaybe)

charToInt :: Char -> Int
charToInt '(' = 1
charToInt _ = -1

part1Solution :: String -> Int
part1Solution = foldl (\n -> (n +) . charToInt) 0

part2Solution :: String -> Int
part2Solution = fst . head . filter ((< 0) . snd) . scanl (\(fl, nextFl) c -> (fl + 1, nextFl + charToInt c)) (0, 0)
