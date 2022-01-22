module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (guard)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

-- This function can also be just arithmetic.
-- like 4 * n * n + 4 * n + (1 - x) = 0
-- truncated positive root of this equaton (for n, x is defined) is result
-- but I feel lazy today
squares :: Int -> Int
squares x = fst . head . filter ((> x) . snd) . map (\n -> (n - 1, 4 * n * (n + 1) + 2)) $ [0 ..]

lastInSquare :: Int -> Int
lastInSquare n = 1 + 4 * n * (n + 1)

part1Solution :: Int -> Int
part1Solution x = n + 1 + (minimum . map (abs . (x -))) [m + n + 1, m + 3 * n + 3, m + 5 * n + 5, m + 7 * n + 7]
  where
    n = squares x
    m = lastInSquare n

xyToN :: Int -> Int -> Int
xyToN x y
  | abs x > abs y && x >= 0 = y + lastInSquare (x - 1) + x
  | abs x > abs y && x < 0 = lastInSquare ((- x) - 1) - y - 5 * x
  | abs x <= abs y && y >= 0 = 3 * y + lastInSquare (y - 1) - x
  | abs x <= abs y && y < 0 = x + lastInSquare ((- y) - 1) - 7 * y

nToXY :: Int -> (Int, Int)
nToXY n
  | n <= m + 2 * k + 2 = (k + 1, n - m - k - 1)
  | n <= m + 4 * k + 4 = (3 * k + 3 + m - n, k + 1)
  | n <= m + 6 * k + 6 = ((- k) - 1, 5 * k + 5 + m - n)
  | n <= m + 8 * k + 8 = (n - m - k * 7 - 7, (- k) - 1)
  where
    k = squares n
    m = lastInSquare k

memory :: [Int]
memory = map doer [0 ..]
  where
    doer :: Int -> Int
    doer 0 = 0
    doer 1 = 1
    doer n = sum $ do
      dx <- [-1 .. 1]
      dy <- [-1 .. 1]
      guard $ dx /= 0 || dy /= 0
      let (x, y) = nToXY n
          x' = x + dx
          y' = y + dy
          n' = xyToN x' y'
      guard $ n' < n
      return $ memory !! n'

part2Solution :: Int -> Int
part2Solution x = head . filter (> x) $ memory
