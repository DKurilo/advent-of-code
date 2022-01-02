module Lib
    ( part1solution
    , part2solution
    ) where

import           Data.List (elemIndex)

input :: IO [Int]
input = map read . filter (not . null) . lines <$> readFile "input"

magic :: Int
magic = 20201227

loop :: Int -> Int -> Int
loop subject x = (x * subject) `mod` magic

loops :: Int -> [Int]
loops subject = scanl (\n _ -> loop subject n) 1 [0..]

bruteForce :: Int -> Int -> Int
bruteForce subject x = case x `elemIndex` loops subject of
                         Just n -> n
                         _      -> 0

part1solution :: IO ()
part1solution = do
    (x1:x2:_) <- input
    let ls1 = bruteForce 7 x1
    print (loops x2 !! ls1)

part2solution :: IO ()
part2solution = putStrLn "part2solution"
