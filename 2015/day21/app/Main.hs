module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  boss <- getArgs >>= (fmap read . readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1"
  print . part1Solution $ boss
  putStrLn "Part 2"
  print . part2Solution $ boss
