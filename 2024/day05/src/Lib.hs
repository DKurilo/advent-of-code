module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.Map as M
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Data.List (foldl')

parseRule :: String -> (Int, Int)
parseRule cs = (read x, read y)
    where
        (x:y:_) = splitOn "|" cs

parseRules :: [String] -> [(Int, Int)]
parseRules = fmap parseRule

parseUpdate :: String -> M.Map Int Int
parseUpdate = M.fromList . (`zip` [0..]) . fmap read . splitOn ","

parseUpdates :: [String] -> [M.Map Int Int]
parseUpdates = fmap parseUpdate

parseInput :: [String] -> ([(Int, Int)], [M.Map Int Int])
parseInput inp = (parseRules rulesS, parseUpdates updatesS)
    where
        (rulesS : updatesS : _) = splitOn [""] inp

checkRule :: (Int, Int) -> M.Map Int Int -> Bool
checkRule (x, y) update = case (x `M.lookup` update, y `M.lookup` update) of
    (Just p1, Just p2)
        | p1 >= p2 -> False
    _ -> True

middle :: M.Map Int Int -> Int
middle update = fst . head . filter ((==middlePos) . snd) $ pairs
    where
        pairs = M.toList update
        maxPos = maximum . fmap snd $ pairs
        middlePos = maxPos `div` 2

part1Solution :: [String] -> Int
part1Solution inp = sum . fmap middle . filter (\upd -> all (`checkRule` upd) rules) $ updates
    where
        (rules, updates) = parseInput inp

applyRule :: (Int, Int) -> M.Map Int Int -> M.Map Int Int
applyRule (x, y) update = case (x `M.lookup` update, y `M.lookup` update) of
    (Just p1, Just p2)
        | p1 >= p2 -> M.map (\pos -> if pos == p1
                                       then p2
                                       else if pos == p2
                                              then p2 + 1
                                              else if pos < p2 || pos > p1
                                                     then pos
                                                     else pos + 1) update
    _ -> update

part2Solution :: [String] -> Int
part2Solution inp = sum
                  . fmap ( middle 
                         . head
                         . dropWhile (\upd -> not (all (`checkRule` upd) rules))
                         . iterate (\upd -> foldl' (flip applyRule) upd rules)
                         )
                  . filter (\upd -> not (all (`checkRule` upd) rules))
                  $ updates
    where
        (rules, updates) = parseInput inp
