module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Arrow = L | R deriving (Show)

parseLine :: String -> (Arrow, Int)
parseLine [] = (L, 0)
parseLine [_] = (L, 0)
parseLine (c:cs) = (if c == 'L' then L else R, read cs)

dirToSign :: (Arrow, Int) -> Int
dirToSign (L, x) = -x
dirToSign (R, x) = x

part1Solution :: [String] -> Int
part1Solution = snd
              . foldl (\(x, r) cs -> let x' = (x + (dirToSign . parseLine) cs) `mod` 100 in (x', if x' == 0 then r + 1 else r)) (50, 0)

part2Solution :: [String] -> Int
part2Solution = snd
              . foldl (\(x, r) cs -> let clicks = (dirToSign . parseLine) cs
                                         pos = x + clicks
                                         x' = pos `mod` 100
                                         y = abs pos `div` 100 + (if x > 0 && pos <= 0 then 1 else 0)
                                     in (x', r + y)) (50, 0)
