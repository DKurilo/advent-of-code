module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

next :: (Int -> Int -> Int -> Bool) -> [[Bool]] -> [[Bool]]
next p grid = [[nextState i k | k <- [0 .. n]] | i <- [0 .. n]]
  where
    n = length grid - 1
    nextState i k
      | p n i k = True
      | l && nls == 2 = True
      | nls == 3 = True
      | otherwise = False
      where
        l = grid !! i !! k
        nls =
          length
            . filter id
            $ [ grid !! i' !! k'
                | x <- [-1 .. 1],
                  y <- [-1 .. 1],
                  x /= 0 || y /= 0,
                  let i' = i + x,
                  let k' = k + y,
                  not (i' < 0 || k' < 0 || i' > n || k' > n)
              ]

part1Solution :: [[Bool]] -> Int
part1Solution = length . filter id . concat . (!! 100) . iterate (next (\_ _ _ -> False))

corners :: Int -> Int -> Int -> Bool
corners n i k
  | i == 0 && k == 0 = True
  | i == 0 && k == n = True
  | i == n && k == 0 = True
  | i == n && k == n = True
  | otherwise = False

turnOnSome :: (Int -> Int -> Int -> Bool) -> [[Bool]] -> [[Bool]]
turnOnSome p grid = zipWith (\i -> zipWith (\k l -> p n i k || l) [0 ..]) [0 ..] grid
  where
    n = length grid - 1

part2Solution :: [[Bool]] -> Int
part2Solution = length . filter id . concat . (!! 100) . iterate (next corners) . turnOnSome corners
