module Main where

import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution, mkPlan)
import System.Environment (getArgs)

main :: IO ()
main = do
    plan <- getArgs >>= (fmap mkPlan . readFile . fromMaybe "./input" . listToMaybe)
    putStrLn "Part 1"
    print . part1Solution $ plan
    putStrLn "Part 2"
    print . part2Solution $ plan
