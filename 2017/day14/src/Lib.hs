module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (guard)
import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl', foldl1')
import qualified Data.Set as S
import Debug.Trace (trace)
import Numeric (showHex)

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

knotHash :: String -> String
knotHash cs = denseHash . (\(ns, _, _) -> ns) . (!! 64) . iterate (\state -> foldl' transform state ls) $ ([0 .. 255], 0, 0)
  where
    ls = map ord cs ++ [17, 31, 73, 47, 23]

hexCharToBools :: Char -> [Bool]
hexCharToBools '0' = [False, False, False, False]
hexCharToBools '1' = [False, False, False, True]
hexCharToBools '2' = [False, False, True, False]
hexCharToBools '3' = [False, False, True, True]
hexCharToBools '4' = [False, True, False, False]
hexCharToBools '5' = [False, True, False, True]
hexCharToBools '6' = [False, True, True, False]
hexCharToBools '7' = [False, True, True, True]
hexCharToBools '8' = [True, False, False, False]
hexCharToBools '9' = [True, False, False, True]
hexCharToBools 'a' = [True, False, True, False]
hexCharToBools 'b' = [True, False, True, True]
hexCharToBools 'c' = [True, True, False, False]
hexCharToBools 'd' = [True, True, False, True]
hexCharToBools 'e' = [True, True, True, False]
hexCharToBools 'f' = [True, True, True, True]

buildMatrix :: String -> [[Bool]]
buildMatrix cs = map (concatMap hexCharToBools . knotHash . (cs ++) . ('-' :) . show) [0 .. 127]

getRegion :: (Int, Int) -> [[Bool]] -> S.Set (Int, Int)
getRegion coord mx = doer S.empty coord
  where
    h = length mx
    w = length . head $ mx
    doer marked coord'@(i, j)
      | j < 0 || j >= h = marked
      | i < 0 || i >= w = marked
      | coord' `S.member` marked = marked
      | not (mx !! j !! i) = marked
      | otherwise = foldl' doer (S.insert coord' marked) [(i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1)]

countRegions :: [[Bool]] -> Int
countRegions mx = fst . foldl' doer (0, S.empty) $ [(i, j) | i <- [0 .. w - 1], j <- [0 .. h - 1]]
  where
    h = length mx
    w = length . head $ mx
    doer (regs, marked) coord@(i, j)
      | coord `S.member` marked = (regs, marked)
      | not (mx !! j !! i) = (regs, marked)
      | otherwise = (regs + 1, marked `S.union` getRegion coord mx)

part1Solution :: String -> Int
part1Solution = length . filter id . concat . buildMatrix

part2Solution :: String -> Int
part2Solution = countRegions . buildMatrix
