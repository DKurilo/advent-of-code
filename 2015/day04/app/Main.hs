{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  bs <- getArgs >>= (fmap (head . BC.lines) . B.readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1"
  print . part1Solution $ bs
  putStrLn "Part 2"
  print . part2Solution $ bs
