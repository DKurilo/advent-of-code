module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (scanl')
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

part1Solution :: [Int] -> Int
part1Solution = sum

freqs :: [Int] -> [Int]
freqs = scanl' (+) 0 . cycle

part2Solution :: [Int] -> Int
part2Solution dfs = fs !! (length . takeWhile (\(i, f) -> f `notElem` take i fs) . zip [0 ..] $ fs)
  where
    fs = freqs dfs
