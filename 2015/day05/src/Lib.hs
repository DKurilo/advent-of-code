module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

vowels = "aeiou"

naughty = ["ab", "cd", "pq", "xy"]

isNice1 cs = length vs >= 3 && (not . null) ds && null ns
  where
    vs = filter (`elem` vowels) cs
    ps = zip cs (drop 1 cs)
    ds = filter (uncurry (==)) ps
    ns = filter (\(c1, c2) -> [c1, c2] `elem` naughty) ps

part1Solution :: [String] -> Int
part1Solution = length . filter isNice1

isNice2 cs = any (\n -> (ps !! n) `elem` drop (n + 2) ps) [0 .. (length ps - 2)] && (not . null) ts
  where
    ps = zip cs (drop 1 cs)
    ts = filter (uncurry (==)) . zip cs . drop 2 $ cs

part2Solution :: [String] -> Int
part2Solution = length . filter isNice2
