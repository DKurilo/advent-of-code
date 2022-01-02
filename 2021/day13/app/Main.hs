module Main where

import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set (..), fromList)
import Lib (Paper (..), part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  ls <- getArgs >>= (fmap lines . readFile . fromMaybe "./input" . listToMaybe)
  let (paper, folds) = bimap (Paper . fromList . map read) (map read) . (\[ds, fs] -> (ds, fs)) . splitOn [""] $ ls
  putStrLn "Part 1"
  print $ part1Solution folds paper
  putStrLn "Part 2"
  putStrLn $ part2Solution folds paper
