module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (parse, solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  cave <- getArgs >>= (fmap parse . readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1/2"
  print . solution $ cave
