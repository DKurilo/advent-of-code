module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (elemIndex, foldl')
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

decryptionKey :: Int
decryptionKey = 811589153

mixNumbers :: [(Int, Int)] -> [(Int, Int)]
mixNumbers xs = foldl' doer xs [0 .. length xs - 1]
  where
    doer :: [(Int, Int)] -> Int -> [(Int, Int)]
    doer ixs i = case i `elemIndex` map fst ixs of
      Just k
        | k' == 0 -> ixs
        | k' >= k -> take k ixs <> (take (k' - k) . drop (k + 1)) ixs <> [ix] <> drop (k' + 1) ixs
        | otherwise -> take k' ixs <> [ix] <> (take (k - k') . drop k') ixs <> drop (k + 1) ixs
        where
          ix = ixs !! k
          x = snd ix
          k'
            | x == 0 = k
            | otherwise = (k + x - 1) `mod` (l - 1) + 1
      _ -> error $ "unknown index " <> show i
      where
        l = length ixs

part1Solution :: [Int] -> Int
part1Solution xs = sum . map (\k -> mixed !! ((k + zero) `mod` l)) $ [1000, 2000, 3000]
  where
    l = length xs
    mixed = map snd . mixNumbers . zip [0 ..] $ xs
    zero = fromMaybe 0 . elemIndex 0 $ mixed

part2Solution :: [Int] -> Int
part2Solution xs = sum . map (\k -> mixed !! ((k + zero) `mod` l)) $ [1000, 2000, 3000]
  where
    l = length xs
    mixed = map snd . (!! 10) . iterate mixNumbers . zip [0 ..] . map (* decryptionKey) $ xs
    zero = fromMaybe 0 . elemIndex 0 $ mixed
