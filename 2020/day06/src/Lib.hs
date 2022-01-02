module Lib
    ( part1solution
    , part2solution
    ) where

import           Data.List.Split (splitOn)
import           Data.Set        (Set (..), empty, fromList, intersection, size)

combineAnswers1 :: [String] -> Set Char
combineAnswers1 = foldl (<>) empty . map fromList

combineAnswers2 :: [String] -> Set Char
combineAnswers2 = foldl intersection (fromList ['a'..'z']) . map fromList

input :: ([String] -> Set Char) -> IO [Set Char]
input combiner = map (combiner . lines) . splitOn "\n\n" <$> readFile "./input"

part1solution :: IO ()
part1solution = (print . sum . map size) =<< input combineAnswers1

part2solution :: IO ()
part2solution = (print . sum . map size) =<< input combineAnswers2
