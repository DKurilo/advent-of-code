module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

part1Solution :: [Int] -> Int
part1Solution ns = sum . map fst . filter (uncurry (==)) . zip ns . tail . cycle $ ns

part2Solution :: [Int] -> Int
part2Solution ns = sum . map fst . filter (uncurry (==)) . zip ns . drop (length ns `div` 2) . cycle $ ns
