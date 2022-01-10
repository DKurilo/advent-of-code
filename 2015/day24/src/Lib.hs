module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (sort, subsequences, (\\))
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

exists :: [Int] -> Int -> [Int] -> Bool
exists xs w ws = any (\ws' -> sum ws' == w) . subsequences $ xs \\ ws

exists2 :: [Int] -> Int -> [Int] -> Bool
exists2 xs w ws = any (\ws'' -> sum ws'' == w && exists ws' w ws'') . subsequences $ ws'
  where
    ws' = xs \\ ws

part1Solution :: [Int] -> Int
part1Solution xs = minimum . map product $ passengerWeights
  where
    w = sum xs `div` 3
    passengerWeightsVariants = filter (\xs' -> sum xs' == w) . subsequences $ xs
    smallest = minimum . map length $ passengerWeightsVariants
    passengerWeights = filter (\ws -> length ws == smallest && exists xs w ws) passengerWeightsVariants

part2Solution :: [Int] -> Int
part2Solution xs = minimum . map product $ passengerWeights
  where
    w = sum xs `div` 4
    passengerWeightsVariants = filter (\xs' -> sum xs' == w) . subsequences $ xs
    smallest = minimum . map length $ passengerWeightsVariants
    passengerWeights = filter (\ws -> length ws == smallest && exists2 xs w ws) passengerWeightsVariants
