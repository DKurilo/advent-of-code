module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (nub, nubBy, subsequences)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

primes :: (Integral a) => [a]
primes = 2 : [x | x <- [3 ..], (not . any ((== 0) . (x `mod`)) . takeWhile (<= (x `div` 2))) primes]

isPrime :: (Integral a) => a -> Bool
isPrime x = not . any (\n -> x `mod` n == 0) . takeWhile (\n -> n <= x `div` 2) $ primes

happyHouses :: (Integral a) => [a]
happyHouses = 1 : [prev * (x `div` gcd prev x) | x <- [2 ..], let prev = happyHouses !! (fromIntegral x - 2)]

divisors :: (Integral a) => a -> [a]
divisors n
  | n < 1 = error "only for integral numbers more than 0"
  | n == 1 = [n]
  | otherwise = 1 : [if x == n2 then n else x | let n2 = n `div` 2 + 1, x <- [2 .. n2], n `mod` x == 0 || x == n2]

part1Solution :: (Integral a, Show a) => a -> a -> a
part1Solution k x =
  minimum
    . filter ((>= x) . (* k) . sum . divisors)
    $ [lastHappyHouse `div` n * m | n <- ds, m <- [(n + 1) .. n * (n + 1)]]
  where
    lastHappyHouse =
      last
        . takeWhile ((<= x) . (* k) . sum . divisors)
        $ happyHouses
    ds = filter isPrime . divisors $ lastHappyHouse

presents50 :: (Integral a) => a -> a -> a
presents50 k x = (* k) . sum . dropWhile (\n -> x `div` n >= 50) . divisors $ x

part2Solution :: (Integral a, Show a) => a -> a -> a
part2Solution k x =
  minimum
    . filter ((>= x) . presents50 k)
    $ [lastHappyHouse `div` n * m | n <- ds, m <- [(n + 1) .. n * (n + 1)]]
  where
    lastHappyHouse =
      last
        . takeWhile ((<= x) . presents50 k)
        $ happyHouses
    ds = filter isPrime . divisors $ lastHappyHouse
