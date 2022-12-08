module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (transpose)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data MaxHeight = MaxHeight
  { ownHeight :: Int,
    fromLeft :: Int,
    fromTop :: Int,
    fromRight :: Int,
    fromBottom :: Int
  }
  deriving (Show)

getMaxHeights :: [[Int]] -> [[MaxHeight]]
getMaxHeights grid =
  [ [ MaxHeight (l !! i) (maxX i l) (maxX j l') (maxX (w - i - 1) (reverse l)) (maxX (h - j - 1) (reverse l'))
      | i <- [0 .. w - 1],
        let l' = grid' !! i
    ]
    | j <- [0 .. h - 1],
      let l = grid !! j
  ]
  where
    grid' = transpose grid
    h = length grid
    w = length . head $ grid
    maxX n xs
      | n == 0 = -1
      | otherwise = maximum . take n $ xs

isVisible :: MaxHeight -> Bool
isVisible tree = h > fromLeft tree || h > fromTop tree || h > fromRight tree || h > fromBottom tree
  where
    h = ownHeight tree

part1Solution :: [[Int]] -> Int
part1Solution = length . filter id . concatMap (map isVisible) . getMaxHeights

part2Solution :: [[Int]] -> Int
part2Solution grid =
  maximum
    [ visibleTrees l i * visibleTrees l' j * visibleTrees (reverse l) (w - i - 1) * visibleTrees (reverse l') (h - j - 1)
      | j <- [0 .. h - 1],
        let l = grid !! j,
        i <- [0 .. w - 1],
        let l' = grid' !! i
    ]
  where
    grid' = transpose grid
    w = length grid
    h = length . head $ grid
    visibleTrees xs i
      | lCanSee == i = lCanSee
      | otherwise = lCanSee + 1
      where
        ownH = xs !! i
        canSee = takeWhile (< ownH) . reverse . take i $ xs
        lCanSee = length canSee
