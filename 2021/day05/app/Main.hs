module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (isHorVer, isHorVerDiag, part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <- getArgs >>= (fmap (map read . lines) . readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1"
  print . part1Solution . filter isHorVer $ xs
  putStrLn "Part 2"
  print . part2Solution . filter isHorVerDiag $ xs
