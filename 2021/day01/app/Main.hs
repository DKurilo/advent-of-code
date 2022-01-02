module Main where

import Lib (part1Solution, part2Solution)

main :: IO ()
main = do
  putStrLn "Part 1"
  part1Solution >>= putStrLn
  putStrLn "Part 2"
  part2Solution >>= putStrLn
