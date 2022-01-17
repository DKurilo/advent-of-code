module Main where

import Data.ByteString.Char8 (pack)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  passcode <- pack . head <$> getArgs
  putStrLn "Part 1"
  print . part1Solution $ passcode
  putStrLn "Part 2"
  print . part2Solution $ passcode
