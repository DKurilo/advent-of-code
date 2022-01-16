module Main where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  cs <- getArgs >>= (fmap (filter (not . isSpace)) . readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1"
  print . part1Solution . read $ cs
  putStrLn "Part 2"
  print . part2Solution . read $ cs
