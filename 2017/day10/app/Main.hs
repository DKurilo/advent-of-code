module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  ls <- getArgs >>= (fmap lines . readFile . fromMaybe "./input" . listToMaybe)
  let (ncs : cs : _) = ls
      n = read ncs
      ns = map read . words . map (\c -> if c == ',' then ' ' else c) $ cs
  putStrLn "Part 1"
  print $ part1Solution n ns
  putStrLn "Part 2"
  print $ part2Solution cs
