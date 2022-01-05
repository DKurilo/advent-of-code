module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

abc = ['a' .. 'z']

incPass :: String -> String
incPass = reverse . doer True . reverse
  where
    doer _ "" = ""
    doer t (c : cs) = abc !! i' : doer t' cs
      where
        i = fromMaybe 0 (elemIndex c abc) + if t then 1 else 0
        labc = length abc
        t' = i >= labc
        i' = i `mod` labc

isValid :: String -> Bool
isValid cs = length cs == 8 && null bad && hasTriples && (length . take 2) pairs == 2
  where
    bad = filter (\c -> c == 'i' || c == 'o' || c == 'l') cs
    ind c = fromMaybe 0 (elemIndex c abc)
    hasTriples =
      or
        . zipWith
          (\c1 (c2, c3) -> let start = ind c1 in ind c2 == start + 1 && ind c3 == start + 2)
          cs
        . zip (tail cs)
        . drop 2
        $ cs
    pairs =
      fst
        . foldl
          ( \(ps, prev) (c1, c2) ->
              let p = [c1, c2]
               in if c1 == c2 && p /= prev
                    then (p : ps, p)
                    else (ps, "")
          )
          ([], "")
        . zip cs
        . tail
        $ cs

part1Solution :: String -> String
part1Solution = until isValid incPass . incPass

part2Solution :: String -> String
part2Solution = part1Solution . part1Solution
