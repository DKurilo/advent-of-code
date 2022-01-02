module Main where

import Lib (part1Solution, part2Solution)

main :: IO ()
main = do
  commands <- map read . lines <$> readFile "./input"
  putStrLn "Part 1"
  print . part1Solution $ commands
  putStrLn "Part 2"
  print . part2Solution $ commands
