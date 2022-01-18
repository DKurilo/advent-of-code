module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (replicate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

steal1 :: [a] -> [a]
steal1 [] = []
steal1 [_] = []
steal1 (x1 : _ : xs) = x1 : steal1 xs

play1 :: [a] -> a
play1 [x] = x
play1 xs
  | (even . length) xs = play1 . steal1 $ xs
  | otherwise = play1 . (last xs :) . steal1 $ xs

part1Solution :: Int -> Int
part1Solution n = play1 [1 .. n]

play2 :: M.Map Int Int -> Int
play2 rels = doer 1 (startY, startY + 1) rels
  where
    startY = M.size rels `div` 2
    doer x (y', y) rels
      | M.size rels == 1 = x
      | otherwise = doer x' nextVictim rels'
      where
        x' = fromMaybe 0 . M.lookup x $ rels'
        nextY = fromMaybe 0 . M.lookup y $ rels
        nextY' = fromMaybe 0 . M.lookup nextY $ rels
        rels' = M.insert y' nextY . M.delete y $ rels
        nextVictim = if (odd . M.size) rels then (nextY, nextY') else (y', nextY)

part2Solution :: Int -> Int
part2Solution n = play2 . M.fromList $ [(i, if i < n then i + 1 else 1) | i <- [1 .. n]]
