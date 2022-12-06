module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (nub, zipWith4)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

isUnique :: String -> Bool
isUnique cs = length cs == (length . nub) cs

prefixes :: Int -> String -> [String]
prefixes n cs = doer (length cs) cs
  where
    doer m cs'
      | m < n = []
      | otherwise = take n cs' : doer (m - 1) (tail cs')

solution :: Int -> String -> Int
solution n = (n +) . fst . head . filter (isUnique . snd) . zip [0 ..] . prefixes n

part1Solution :: String -> Int
part1Solution = solution 4

part2Solution :: String -> Int
part2Solution = solution 14
