module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (first)
import Data.List (foldl1')
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

reallocate :: [Int] -> [Int]
reallocate xs =
  fst . (!! x)
    . iterate (\(xs', j) -> (take j xs' ++ (((xs' !! j) + 1) : drop (j + 1) xs'), (j + 1) `mod` l))
    $ (take i xs ++ (0 : drop (i + 1) xs), (i + 1) `mod` l)
  where
    (i, x) = foldl1' (\mxp xp -> if snd xp > snd mxp then xp else mxp) . zip [0 ..] $ xs
    l = length xs

findLoopStep :: [Int] -> (Int, [Int])
findLoopStep = doer []
  where
    doer prevs xs
      | xs `elem` prevs = (0, xs)
      | otherwise = first (1 +) . (doer (xs : prevs) . reallocate) $ xs

part1Solution :: [Int] -> Int
part1Solution = fst . findLoopStep

part2Solution :: [Int] -> Int
part2Solution = fst . findLoopStep . snd . findLoopStep
