{-# LANGUAGE TupleSections #-}

module Main where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- getArgs >>= (fmap lines . readFile . fromMaybe "./input" . listToMaybe)
  let ns = map read . splitOn "," . head $ input
      boards = map (map (map ((,False) . read) . words)) . splitOn [""] . drop 2 $ input
  putStrLn "Part 1"
  print $ part1Solution ns boards
  putStrLn "Part 2"
  print $ part2Solution ns boards
