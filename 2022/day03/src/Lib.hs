module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (elemIndex, foldl', nub)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

items :: String
items = ['a' .. 'z'] <> ['A' .. 'Z']

data Rucksack = Rucksack String String

parseRucksack :: String -> Rucksack
parseRucksack cs = Rucksack p1 p2
  where
    l = length cs `div` 2
    p1 = take l cs
    p2 = drop l cs

score :: Char -> Int
score = (+ 1) . fromMaybe 0 . (`elemIndex` items)

wrongItems :: Rucksack -> String
wrongItems (Rucksack cs1 cs2) = nub . foldl' (\cs c -> if c `elem` cs2 then c : cs else cs) "" $ cs1

part1Solution :: [String] -> Int
part1Solution = sum . map score . concatMap (wrongItems . parseRucksack)

data ElvenGroup = Elves String String String

elvenGroups :: [String] -> [ElvenGroup]
elvenGroups [] = []
elvenGroups [_] = []
elvenGroups [_, _] = []
elvenGroups (cs1 : cs2 : cs3 : rest) = Elves cs1 cs2 cs3 : elvenGroups rest

commonForGroup :: ElvenGroup -> String
commonForGroup (Elves cs1 cs2 cs3) = nub . foldl' (\cs c -> if c `elem` cs2 && c `elem` cs3 then c : cs else cs) "" $ cs1

part2Solution :: [String] -> Int
part2Solution = sum . map score . concatMap commonForGroup . elvenGroups
