module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.List (transpose)
import Data.List.Split (splitOn)
import qualified Data.Set as S

newtype Key = Key
  { unkey :: [Int]
  } deriving (Show, Eq, Ord)

parseKey :: [String] -> Key
parseKey = Key . fmap (length . filter (== '#')) . transpose . tail . init

newtype Lock = Lock
  { unlock :: [Int]
  } deriving (Show, Eq, Ord)

parseLock :: [String] -> Lock
parseLock = Lock . fmap (length . filter (== '#')) . transpose . tail . init

isLock :: [String] -> Bool
isLock = (== "#####") . head

areFit :: Lock -> Key -> Bool
areFit l = all (<= 5) . zipWith (+) (unlock l) . unkey

part1Solution :: [String] -> Int
part1Solution css =
  S.size . S.fromList $ [(l, k) | l <- locks, k <- keys, areFit l k]
  where
    schemas = splitOn [""] css
    locks = fmap parseLock . filter isLock $ schemas
    keys = fmap parseKey . filter (not . isLock) $ schemas

part2Solution :: [String] -> Int
part2Solution = length
