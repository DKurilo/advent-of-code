module Main where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (readMany, solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <- getArgs >>= (fmap (map read . splitOn "\n\n") . readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1 / 2"
  print . solution $ xs
