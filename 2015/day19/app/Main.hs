module Main where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  (mol : _ : css) <- getArgs >>= (fmap (reverse . lines) . readFile . fromMaybe "./input" . listToMaybe)
  let rules = M.fromListWith (++) . map ((\[from, to] -> (from, [to])) . splitOn " => ") $ css
  putStrLn "Part 1"
  print . part1Solution rules $ mol
  putStrLn "Part 2"
  print . part2Solution rules $ mol
