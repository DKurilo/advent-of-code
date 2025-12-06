module Lib (
    part1Solution,
    part2Solution,
)
where

import Data.List (transpose)
import Data.List.Split (splitOn)

data Op = Mul [Integer] | Add [Integer] deriving (Eq, Show)

parse1 :: String -> [Op]
parse1 cs = zipWith (\opStr xs -> (if opStr == "*" then Mul else Add) xs) ops ns
  where
    ls = fmap words . lines $ cs
    ns = transpose . fmap (fmap (toInteger . read)) . init $ ls
    ops = last ls

applyOp :: Op -> Integer
applyOp (Add xs) = sum xs
applyOp (Mul xs) = product xs

part1Solution :: String -> Integer
part1Solution = sum . fmap applyOp . parse1

parse2 :: String -> [Op]
parse2 cs = zipWith (\opStr xs -> (if opStr == "*" then Mul else Add) xs) ops ns
  where
    ls = lines cs
    ns' = transpose . init $ ls
    sep = (`replicate` ' ') . length . head $ ns'
    ns = fmap (fmap (toInteger . read . filter (/= ' '))) . splitOn [sep] $ ns'
    ops = words . last $ ls

part2Solution :: String -> Integer
part2Solution = sum . fmap applyOp . parse2
