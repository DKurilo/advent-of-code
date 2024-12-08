module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

part1Solution :: [String] -> Int
part1Solution = length

part2Solution :: [String] -> Int
part2Solution = length
