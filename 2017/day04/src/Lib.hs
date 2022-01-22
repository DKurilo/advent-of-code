module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (permutations)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

allWordsUnique :: [String] -> Bool
allWordsUnique css = null [() | i <- [0 .. length css - 2], j <- [i + 1 .. length css - 1], css !! i == css !! j]

isAnagram :: String -> String -> Bool
isAnagram cs1 = elem cs1 . permutations

hasNoAnagrams :: [String] -> Bool
hasNoAnagrams css = null [() | i <- [0 .. length css - 2], j <- [i + 1 .. length css - 1], isAnagram (css !! i) (css !! j)]

part1Solution :: [[String]] -> Int
part1Solution = length . filter allWordsUnique

part2Solution :: [[String]] -> Int
part2Solution = length . filter (\css -> allWordsUnique css && hasNoAnagrams css)
