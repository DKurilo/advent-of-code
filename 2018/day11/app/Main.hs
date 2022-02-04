module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <- read . head <$> getArgs
  putStrLn "Part 1"
  print . part1Solution $ xs
  putStrLn "Part 2"
  print . part2Solution $ xs
