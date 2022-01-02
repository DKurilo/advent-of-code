module Main where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  (input, rules) <- getArgs >>= (fmap ((\[cs, rs] -> (read cs, read rs)) . splitOn "\n\n") . readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1"
  print $ part1Solution rules input
  putStrLn "Part 2"
  print $ part2Solution rules input
