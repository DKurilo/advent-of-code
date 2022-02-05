module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <- head <$> getArgs
  putStrLn "Part 1"
  putStrLn . part1Solution . read $ xs
  putStrLn "Part 2"
  print . part2Solution $ xs
