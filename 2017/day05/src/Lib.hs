module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

jump :: (Int -> Int) -> Int -> [Int] -> (Int, [Int])
jump f pos js = (pos + j, take pos js ++ (f j : drop (pos + 1) js))
  where
    j = js !! pos

stepsToExit :: (Int -> Int) -> [Int] -> Int
stepsToExit f = doer 0
  where
    doer :: Int -> [Int] -> Int
    doer pos js
      | pos < 0 || pos >= length js = 0
      | otherwise = 1 + doer pos' js'
      where
        (pos', js') = jump f pos js

part1Solution :: [Int] -> Int
part1Solution = stepsToExit (+ 1)

part2Solution :: [Int] -> Int
part2Solution = stepsToExit (\j -> if j < 3 then j + 1 else j - 1)
