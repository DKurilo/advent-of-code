module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

factors :: [Int]
factors = [16807, 48271]

base :: Int
base = 2147483647

generate :: (Int, Int, Int) -> (Int, Int, Int)
generate (x, factor, b) = ((x * factor) `mod` b, factor, b)

last16bit :: Int -> Int
last16bit x = x `mod` 65536 -- 2**16

part1Solution :: [Int] -> Int
part1Solution =
  length
    . filter (\[(x1, _, _), (x2, _, _)] -> last16bit x1 == last16bit x2)
    . take 40000000
    . drop 1
    . iterate (map generate)
    . zipWith (\f x -> (x, f, base)) factors

stream :: Int -> Int -> Int -> Int -> [Int]
stream x f b m = filter ((== 0) . (`mod` m)) . drop 1 . map (\(x', _, _) -> x') . iterate generate $ (x, f, b)

part2Solution :: [Int] -> Int
part2Solution [x1, x2] =
  length
    . filter (\(x1', x2') -> last16bit x1' == last16bit x2')
    . take 5000000
    $ zip (stream x1 (head factors) base 4) (stream x2 (last factors) base 8)
