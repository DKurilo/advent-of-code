module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl')
import qualified Data.Map as M
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

lhist :: String -> M.Map Char Int
lhist = M.unionsWith (+) . map (`M.singleton` 1)

part1Solution :: [String] -> Int
part1Solution =
  uncurry (*)
    . foldl'
      ( \(tws, ths) cs ->
          let ns = (M.elems . lhist) cs
           in case (2 `elem` ns, 3 `elem` ns) of
                (False, False) -> (tws, ths)
                (True, False) -> (tws + 1, ths)
                (False, True) -> (tws, ths + 1)
                (True, True) -> (tws + 1, ths + 1)
      )
      (0, 0)

findCommons :: String -> String -> (String, Int)
findCommons [] [] = ("", 0)
findCommons cs [] = (cs, length cs)
findCommons [] cs = (cs, length cs)
findCommons (c1 : cs1) (c2 : cs2)
  | c1 == c2 = (c1 : common, diff)
  | otherwise = (common, 1 + diff)
  where
    (common, diff) = findCommons cs1 cs2

part2Solution :: [String] -> String
part2Solution css = head [common | cs1 <- css, cs2 <- css, let (common, diff) = findCommons cs1 cs2, diff == 1]
