module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <- getArgs >>= (fmap (map read . lines) . readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1"
  print . part1Solution $ xs
