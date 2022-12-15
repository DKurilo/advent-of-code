module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  ([n, from, to], xs) <-
    getArgs
      >>= ( fmap ((\xs' -> (read . last $ xs', map read . init $ xs')) . lines)
              . readFile
              . fromMaybe "./input"
              . listToMaybe
          )
  putStrLn "Part 1"
  print . part1Solution n $ xs
  putStrLn "Part 2"
  print . part2Solution from to $ xs
