module Main where

import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <- getArgs >>= (fmap (map read . lines) . readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1"
  print . part1Solution $ xs
  putStrLn "Part 2"
  putStrLn . intercalate "\n" . map show . filter (isPrefixOf "north" . fst) . part2Solution $ xs
