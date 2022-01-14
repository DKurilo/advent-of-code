module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (first)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Direction = U | D | L | R deriving (Show, Eq)

instance Read Direction where
  readPrec = lift $ (P.char 'U' >> return U) P.+++ (P.char 'D' >> return D) P.+++ (P.char 'L' >> return L) P.+++ (P.char 'R' >> return R)

nextNumber :: Int -> [Direction] -> Int
nextNumber n [] = n
nextNumber n (U : ds) = let n' = n - 3 in nextNumber (if n' <= 0 then n else n') ds
nextNumber n (D : ds) = let n' = n + 3 in nextNumber (if n' > 9 then n else n') ds
nextNumber n (L : ds) = let n' = n - 1 in nextNumber (if n' `mod` 3 == 0 then n else n') ds
nextNumber n (R : ds) = nextNumber (if n `mod` 3 == 0 then n else n + 1) ds

part1Solution :: [[Direction]] -> Int
part1Solution = fst . foldl (\(code, prev) ds -> let n = nextNumber prev ds in (code * 10 + n, n)) (0, 5)

keyPad2 (x, y) = ["0000000", "0001000", "0023400", "0567890", "00ABC00", "000D000", "0000000"] !! y !! x

nextNumber2 :: (Int, Int) -> [Direction] -> (Char, (Int, Int))
nextNumber2 coords [] = (keyPad2 coords, coords)
nextNumber2 coords@(x, y) (d : ds)
  | keyPad2 (x', y') == '0' = nextNumber2 coords ds
  | otherwise = nextNumber2 (x', y') ds
  where
    x' = case d of
      L -> x - 1
      R -> x + 1
      _ -> x
    y' = case d of
      U -> y - 1
      D -> y + 1
      _ -> y

part2Solution :: [[Direction]] -> String
part2Solution = reverse . fst . foldl (\(code, prev) -> first (: code) . nextNumber2 prev) ("", (1, 3))
