module Lib (
    part1Solution,
    part2Solution,
    parseInput,
)
where

import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M

getRange :: String -> (Int, Int)
getRange cs = (x, y)
  where
    (x : y : _) = fmap read . splitOn "-" $ cs

parseInput :: [String] -> (M.Map Int Int, [Int])
parseInput css = (M.fromList rs, xs)
  where
    (ranges : numbers : _) = splitOn [""] css
    rs = foldl' addToRange [] . sort . fmap getRange $ ranges
    xs = read <$> numbers

    addToRange :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    addToRange [] r = [r]
    addToRange allrs@(r'@(x', y') : rs) r@(x, y)
        | x > y' = r : allrs
        | y <= y' = allrs
        | otherwise = (x', y) : rs

part1Solution :: (M.Map Int Int, [Int]) -> Int
part1Solution (rs, xs) =
    length
        . filter
            ( \x -> case M.lookupLE x rs of
                Just (x', y') -> y' >= x
                _ -> False
            )
        $ xs

part2Solution :: (M.Map Int Int, [Int]) -> Int
part2Solution = sum . fmap (\(x, y) -> y - x + 1) . M.toList . fst
