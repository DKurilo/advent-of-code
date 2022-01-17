module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

triples :: String -> [(Char, Char, Char)]
triples cs = doer (('.' : cs) ++ ".")
  where
    doer [] = []
    doer [c] = []
    doer [c1, c2] = []
    doer (c1 : c2 : c3 : cs) = (c1, c2, c3) : doer (c2 : c3 : cs)

getTile :: (Char, Char, Char) -> Char
getTile ('^', '^', '.') = '^'
getTile ('.', '^', '^') = '^'
getTile ('^', '.', '.') = '^'
getTile ('.', '.', '^') = '^'
getTile _ = '.'

nextRow :: String -> String
nextRow = map getTile . triples

rows :: String -> [String]
rows cs = cs : (rows . nextRow) cs

part1Solution :: [String] -> Int
part1Solution (cs : n : _) = length . filter (== '.') . concat . take (read n) . rows $ cs

part2Solution :: [String] -> Int
part2Solution (cs : _ : n : _) = length . filter (== '.') . concat . take (read n) . rows $ cs
