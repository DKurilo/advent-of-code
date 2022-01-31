module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (toLower)
import Data.Either (either, isRight)
import Data.List (foldl')
import Data.Ord (compare)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

react :: String -> String
react = (\(cs, _, rest) -> rest ++ cs) . foldl' doer ("", False, "")
  where
    doer :: (String, Bool, String) -> Char -> (String, Bool, String)
    doer (cs, reacted, "") c = (cs, reacted, [c])
    doer (cs, reacted, [c']) c
      | c /= c' && toLower c == toLower c' = if null cs then ("", True, "") else (tail cs, True, take 1 cs)
      | otherwise = (c' : cs, reacted, [c])
    doer _ _ = error "it's just impossible"

part1Solution :: String -> Int
part1Solution = length . react

part2Solution :: String -> Int
part2Solution = minimum . zipWith (\c -> length . react . filter (\c' -> c' /= c && toLower c' /= c)) ['a' .. 'z'] . repeat
