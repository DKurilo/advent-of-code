module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', intersect, transpose)
import Data.Maybe (fromMaybe)

splitToPatterns :: [String] -> [[String]]
splitToPatterns = fmap reverse . foldl' (\ps cs -> if null cs then [] : ps else (cs : head ps) : tail ps) [[]]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = (Just . head) xs

findRefPoints :: String -> [Int]
findRefPoints cs
  | length cs < 2 = []
  | otherwise =
      fmap fst
        . filter snd
        . fmap (\n -> (n, and . zipWith (==) (reverse . take n $ cs) . drop n $ cs))
        $ [1 .. length cs - 1]

findSymmetry :: [String] -> [Int]
findSymmetry p = foldl' intersect (head allRefPoints) (tail allRefPoints)
  where
    allRefPoints = fmap findRefPoints p

part1Solution :: [String] -> Int
part1Solution css = verts + horzs
  where
    ps = splitToPatterns css
    verts = sum . fmap (fromMaybe 0 . safeHead . findSymmetry) $ ps
    horzs = sum . fmap ((* 100) . fromMaybe 0 . safeHead . findSymmetry . transpose) $ ps

modifications :: [String] -> [[String]]
modifications p = [modify i j | i <- [0 .. maxI], j <- [0 .. maxJ]]
  where
    maxI = (length . head) p - 1
    maxJ = length p - 1

    modify :: Int -> Int -> [String]
    modify i j = zipWith (\cs j' -> if j' == j then zipWith (\c i' -> if i' == i then invert c else c) cs [0 ..] else cs) p [0 ..]

    invert :: Char -> Char
    invert '.' = '#'
    invert '#' = '.'
    invert c = c

part2Solution :: [String] -> Int
part2Solution css = sum verts + (sum . fmap (* 100)) horzs
  where
    ps = splitToPatterns css
    vertsOrig = fmap (fromMaybe 0 . safeHead . findSymmetry) ps
    horsOrig = fmap (fromMaybe 0 . safeHead . findSymmetry . transpose) ps
    mps = fmap modifications ps
    verts =
      zipWith
        ( \x ps' ->
            let vs = concatMap (filter (/= x) . findSymmetry) ps'
             in if null vs then 0 else head vs
        )
        vertsOrig
        mps
    horzs =
      zipWith
        ( \x ps' ->
            let vs = concatMap (filter (/= x) . findSymmetry . transpose) ps'
             in if null vs then 0 else head vs
        )
        horsOrig
        mps
