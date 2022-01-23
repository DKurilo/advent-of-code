module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl', foldl1')
import Debug.Trace (trace)
import Numeric (showHex)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

transform :: ([Int], Int, Int) -> Int -> ([Int], Int, Int)
transform (ns, pos, skip) l = (ns', (pos + l + skip) `mod` lns, skip + 1)
  where
    lns = length ns
    pns = take l . drop pos . cycle $ ns
    tns = reverse pns
    splTns = lns - (pos `mod` lns)
    tns1 = take splTns tns
    tns2 = drop splTns tns
    ltns2 = length tns2
    ns' = tns2 ++ (take (pos - ltns2) . drop ltns2) ns ++ tns1 ++ drop (pos + l) ns

denseHash :: [Int] -> String
denseHash [] = ""
denseHash ns = cs' ++ denseHash ns'
  where
    n16 = take 16 ns
    ns' = drop 16 ns
    cs = showHex (foldl1' xor n16) ""
    cs' = if length cs == 1 then '0' : cs else cs

part1Solution :: Int -> [Int] -> Int
part1Solution n = (\ns@(n1 : n2 : _, _, _) -> n1 * n2) . foldl' transform ([0 .. n], 0, 0)

part2Solution :: String -> String
part2Solution cs = denseHash . (\(ns, _, _) -> ns) . (!! 64) . iterate (\state -> foldl' transform state ls) $ ([0 .. 255], 0, 0)
  where
    ls = (++ [17, 31, 73, 47, 23]) . map ord $ cs
