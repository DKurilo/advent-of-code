module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  putStrLn "Part 1"
  print (part1Solution 3083 2978)
