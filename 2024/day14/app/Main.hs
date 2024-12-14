module Main where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe, listToMaybe)
import Lib (part1Solution, part2Solution)
import System.Environment (getArgs)

main :: IO ()
main = do
  xs <- getArgs >>= (readFile . fromMaybe "./input" . listToMaybe)
  putStrLn "Part 1"
  print . part1Solution $ xs
  putStrLn "Part 2"
  let part2result = part2Solution xs
      (n, pic) = last part2result
  putStrLn pic
  print n
  -- uncomment for animation
  -- forM_ part2result $ \(n', pic') -> do
  --   putStr $ "\o33[0;0H" <> pic' <> "\n" <> show n' <> "\n"
