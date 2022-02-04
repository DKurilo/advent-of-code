module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Function (on)
import Data.List (maximumBy)
import Data.Ord (compare)
import qualified Data.Vector as V
import Debug.Trace (trace)

powerLevel :: Int -> Int -> Int -> Int
powerLevel n x y = ((((rackId * y + n) * rackId) `div` 100) `mod` 10) - 5
  where
    rackId = x + 10

mkGrid :: Int -> V.Vector (V.Vector Int)
mkGrid n = V.fromList [V.fromList [powerLevel n x y | x <- [1 .. 300]] | y <- [1 .. 300]]

getSum :: V.Vector (V.Vector Int) -> Int -> Int -> Int -> Int -> Int
getSum grid x y dx dy = sum [grid V.! y' V.! x' | x' <- [x .. (x + dx - 1)], y' <- [y .. (y + dy -1)]]

findMax :: V.Vector (V.Vector Int) -> ((Int, Int), Int)
findMax grid = maximumBy (compare `on` snd) [((x + 1, y + 1), getSum grid x y 3 3) | x <- [0 .. maxX], y <- [0 .. maxY]]
  where
    maxY = length grid - 4
    maxX = (V.length . V.head) grid - 4

getSums :: V.Vector (V.Vector Int) -> Int -> Int -> [(Int, Int)]
getSums grid x y = doer 0 0
  where
    lY = length grid
    lX = (V.length . V.head) grid
    maxSize = min (lX - x) (lY - y)
    doer n prev
      | n >= maxSize = []
      | n == 0 = (1, s0) : doer 1 s0
      | otherwise = (n + 1, sn) : doer (n + 1) sn
      where
        s0 = grid V.! y V.! x
        sn =
          prev
            + (sum . map (\dx -> grid V.! (y + n) V.! (x + dx))) [0 .. n]
            + (sum . map (\dy -> grid V.! (y + dy) V.! (x + n))) [0 .. n - 1]

findMaxAnySize :: V.Vector (V.Vector Int) -> ((Int, Int, Int), Int)
findMaxAnySize grid =
  maximumBy
    (compare `on` snd)
    [ ((x + 1, y + 1, size), s)
      | x <- [0 .. maxX],
        y <- [0 .. maxY],
        let (size, s) = maximumBy (compare `on` snd) $ getSums grid x y
    ]
  where
    maxY = length grid - 1
    maxX = (V.length . V.head) grid - 1

part1Solution :: Int -> ((Int, Int), Int)
part1Solution = findMax . mkGrid

part2Solution :: Int -> ((Int, Int, Int), Int)
part2Solution = findMaxAnySize . mkGrid
