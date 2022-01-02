module Main where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (initData, mkSchool, part1Solution, part1Solution', part2Solution, part2Solution')
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <- getArgs >>= (fmap (map read . splitOn ",") . readFile . fromMaybe "./input" . listToMaybe)
  let m = initData xs
  putStrLn "Part 1"
  print . part1Solution $ m
  putStrLn "Part 2"
  print . part2Solution $ m
  let school = mkSchool xs
  putStrLn "Part 1'"
  print . part1Solution' $ school
  putStrLn "Part 2'"
  print . part2Solution' $ school
