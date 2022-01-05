module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (iterate)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

next :: String -> String
next =
  reverse . flush
    . foldl
      ( \p@(cs, cs') c ->
          if null cs' || head cs' == c
            then (cs, c : cs')
            else (flush p, [c])
      )
      ("", "")
  where
    flush (cs, cs') = (reverse . show . length) cs' ++ (head cs' : cs)

part1Solution :: String -> Int
part1Solution = length . reverse . (!! 40) . iterate next . reverse

part2Solution :: String -> Int
part2Solution = length . reverse . (!! 50) . iterate next . reverse
