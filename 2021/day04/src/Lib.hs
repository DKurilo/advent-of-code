module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (join)
import Data.List (transpose)

callNumber :: Int -> [[[(Int, Bool)]]] -> [[[(Int, Bool)]]]
callNumber n = map (map (map (\(n', marked) -> if n' == n then (n', True) else (n', marked))))

isWin :: [[(Int, Bool)]] -> Bool
isWin b = hasRow b || (hasRow . transpose) b
  where
    hasRow = any (all snd)

calculateFirstBoard :: [[[(Int, Bool)]]] -> Int
calculateFirstBoard = sum . map fst . filter (not . snd) . join . head

part1Solution :: [Int] -> [[[(Int, Bool)]]] -> Int
part1Solution ns bs
  | (not . null) winners = n * calculateFirstBoard winners
  | otherwise = part1Solution (tail ns) bs'
  where
    n = head ns
    bs' = callNumber n bs
    winners = filter isWin bs'

part2Solution :: [Int] -> [[[(Int, Bool)]]] -> Int
part2Solution ns bs
  | null losers = n * calculateFirstBoard bs'
  | otherwise = part2Solution (tail ns) losers
  where
    n = head ns
    bs' = callNumber n bs
    losers = filter (not . isWin) bs'
