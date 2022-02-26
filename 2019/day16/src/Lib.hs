module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (foldM, forM_)
import Control.Monad.ST.Lazy (ST (..), runST)
import Data.Foldable (foldlM)
import Data.List (foldl')
import Data.Vector.Unboxed (Vector (..), freeze, (!))
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable (MVector (..), read, slice, write)
import qualified Data.Vector.Unboxed.Mutable as MV
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Message = Message {mS :: String, mSLen :: Int, mL :: Int} deriving (Eq, Show)

patt :: Int -> [Int]
patt n = concatMap (replicate (n + 1)) [0, 1, 0, -1]

patterns :: Int -> [Int]
patterns = drop 1 . cycle . patt

phase :: [Int] -> [Int]
phase xs = map (\n -> (`mod` 10) . abs . sum $ zipWith (*) (patterns n) xs) [0 .. l - 1]
  where
    l = length xs

phase1 :: [Int] -> [Int]
phase1 xs = map (\n -> (`mod` 10) . abs . sum . drop n $ xs) [0 .. l - 1]
  where
    l = length xs

coefs :: Int -> Int -> Vector Int
coefs len depth = runST $
  do
    v <- MV.new (len * depth)
    forM_ [0 .. len - 1] $ \i -> do
      write v i 1
    forM_ [0 .. depth - 1] $ \j -> do
      write v (len * j) 1
    forM_ [1 .. depth - 1] $ \j -> do
      forM_ [1 .. len - 1] $ \i -> do
        n1 <- MV.read v (j * len + i - 1)
        n2 <- MV.read v ((j - 1) * len + i)
        write v (j * len + i) ((n1 + n2) `mod` 10)
    freeze . slice ((depth - 1) * len) len $ v

phase2 :: Vector Int -> [Int] -> [Int]
phase2 cfs xs = trace (show (cfs ! 1000)) $ map (\n -> (`mod` 10) . abs . sum . zipWith (\i x -> x * (cfs ! i)) [0 ..] . drop n $ xs) [0 .. l - 1]
  where
    l = length xs

nsToInt :: [Int] -> Int
nsToInt = foldl' (\w n -> w * 10 + n) 0

part1Solution :: [Int] -> Int
part1Solution = nsToInt . take 8 . (!! 100) . iterate phase

-- for part 2 it's important to notice that only second number from initial pattern affects it
-- then if we have array:
-- n1, n2, n3, n4, n5
-- first number after first phase:
-- n1 + n2 + n3 + n4 + n5, n2 + n3 + n4 + n5, n3 + n4 + n5, n4 + n5, n5
-- n1 + 2n2 + 3n3 + 4n4 + 5n5, n2 + 2n3 + 3n4 + 4n5, n3 + 2n4 + 3n5,...
-- n1 + 3n2 + 6n3 + 10n4 + 15n5,...
-- So to get any element from 100th phase we need coefficients.
-- Let's check coefficients:
-- 1 1 1  1  1  1  1  1  1  1  1
-- 1 2 3  4  5  6  7  8  9 10 11
-- 1 3 6 10 15 21 28 36 45 55 67
-- So each next coefficient is sum of coefficient on top + coefficient on the left.
part2Solution :: [Int] -> Int
part2Solution ns = nsToInt . take 8 . drop 1 . phase2 cfs . drop (offset - 1) . concat . replicate 10000 $ ns
  where
    l = length ns
    offset = nsToInt . take 7 $ ns
    cfs = coefs (10000 * l - offset + 1) 100
