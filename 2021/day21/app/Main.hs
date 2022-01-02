module Main where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (Game (..), Player (..), part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <-
    getArgs
      >>= ( fmap
              ( (\[p1, p2] -> Game False 0 p1 p2)
                  . map (Player 0 . read . last . splitOn ": ")
                  . lines
              )
              . readFile
              . fromMaybe "./input"
              . listToMaybe
          )
  putStrLn "Part 1"
  print . part1Solution $ xs
  putStrLn "Part 2"
  print . part2Solution $ xs
