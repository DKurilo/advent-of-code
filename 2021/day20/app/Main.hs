module Main where

import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Vector (fromList)
import Lib (Image (..), part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  (alg, image) <-
    getArgs
      >>= ( fmap
              ( (\(a : _ : ls) -> (fromList a, (Image '0' . fromList . map fromList) ls)) . lines
                  . map
                    ( \c -> case c of
                        '.' -> '0'
                        '#' -> '1'
                        _ -> c
                    )
              )
              . readFile
              . fromMaybe "./input"
              . listToMaybe
          )
  putStrLn "Part 1"
  print $ part1Solution alg image
  putStrLn "Part 2"
  print $ part2Solution alg image
