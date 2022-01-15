module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Function (on)
import Data.List (maximumBy, minimumBy, sort, transpose)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

bestChar :: (Ord a) => ([(a, Int)] -> (a, Int)) -> [a] -> a
bestChar f xs = fst . f . (\(xs, x) -> x : xs) . foldl doer ([], (head xs, 0)) . sort $ xs
  where
    doer (ls, l@(prev, n)) c
      | prev == c || n == 0 = (ls, (c, n + 1))
      | otherwise = (l : ls, (c, 1))

code :: (Ord a) => ([(a, Int)] -> (a, Int)) -> [[a]] -> [a]
code f = map (bestChar f) . transpose

part1Solution :: [String] -> String
part1Solution = code (maximumBy (compare `on` snd))

part2Solution :: [String] -> String
part2Solution = code (minimumBy (compare `on` snd))
