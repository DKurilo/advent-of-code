module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  x <- getArgs >>= (fmap (read . head . lines) . readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1"
  print . part1Solution 10 $ x
  putStrLn "Part 2"
  print . part2Solution 11 $ x
