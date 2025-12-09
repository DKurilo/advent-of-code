module Lib (
    part1Solution,
    part2Solution,
    parseTile,
)
where

import Data.Function (on)
import Data.List (minimumBy)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

parseTile :: String -> (Int, Int)
parseTile = (\(x : y : _) -> (x, y)) . fmap read . splitOn ","

part1Solution :: [(Int, Int)] -> Int
part1Solution ts =
    maximum
        [ (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
        | i <- [0 .. length ts - 2]
        , let (x1, y1) = ts !! i
        , j <- [i + 1 .. length ts - 1]
        , let (x2, y2) = ts !! j
        ]

data Dir = U | D | L | R deriving (Eq, Show)

nextDs :: Dir -> [Dir]
nextDs U = [L, R]
nextDs D = [L, R]
nextDs L = [U, D]
nextDs R = [U, D]

closestInDir :: (Int, Int) -> Dir -> [(Int, Int)] -> Maybe (Int, Int)
closestInDir (x, y) dir ts
    | dir == U = closest . filter (\(x', y') -> y' < y && x' == x) $ ts
    | dir == D = closest . filter (\(x', y') -> y' > y && x' == x) $ ts
    | dir == L = closest . filter (\(x', y') -> y' == y && x' < x) $ ts
    | otherwise = closest . filter (\(x', y') -> y' == y && x' > x) $ ts
  where
    closest :: [(Int, Int)] -> Maybe (Int, Int)
    closest [] = Nothing
    closest ts' = Just . minimumBy (compare `on` dist) $ ts'
      where
        dist :: (Int, Int) -> (Int, Int)
        dist (x', y') = (abs (x - x'), abs (y - y'))

mkPolys :: [(Int, Int)] -> [[((Int, Int), (Int, Int))]]
mkPolys ts = doer initT ts initDirs
  where
    initT = minimum ts
    initDirs = [R]

    doer :: (Int, Int) -> [(Int, Int)] -> [Dir] -> [[((Int, Int), (Int, Int))]]
    doer startT ts' ds
        | null ts' = [[]]
        | otherwise =
            concatMap
                ( \d -> case closestInDir startT d ts' of
                    Just t' -> fmap ((startT, t') :) . doer t' (filter (/= t') ts') . nextDs $ d
                    Nothing -> []
                )
                ds

isGoodArea :: (Int, Int) -> (Int, Int) -> [((Int, Int), (Int, Int))] -> [(Int, Int)] -> Bool
isGoodArea (x1, y1) (x2, y2) poly ts =
    all isInside [(x1, y1), (x2, y2), (x1, y2), (x2, y1)]
        && all null [h1Intersects, h2Intersects, v1Intersects, v2Intersects]
        && tilesInRect
  where
    hors = filter (\((_, y1'), (_, y2')) -> y1' == y2') poly
    vers = filter (\((x1', _), (x2', _)) -> x1' == x2') poly
    minX = min x1 x2
    maxX = max x1 x2
    minY = min y1 y2
    maxY = max y1 y2

    h1Intersects =
        filter
            ( \((x1', y1'), (_, y2')) ->
                y1 >= min y1' y2'
                    && y1 <= max y1' y2'
                    && minX < x1'
                    && maxX > x1'
                    && ((not . isInside) (x1' + 1, y1) || (not . isInside) (x1' - 1, y1))
            )
            vers
    h2Intersects =
        filter
            ( \((x1', y1'), (_, y2')) ->
                y2 >= min y1' y2'
                    && y2 <= max y1' y2'
                    && minX < x1'
                    && maxX > x1'
                    && ((not . isInside) (x1' + 1, y2) || (not . isInside) (x1' - 1, y2))
            )
            vers
    v1Intersects =
        filter
            ( \((x1', y1'), (x2', _)) ->
                x1 >= min x1' x2'
                    && x1 <= max x1' x2'
                    && minY < y1'
                    && maxY > y1'
                    && ((not . isInside) (x1, y1' + 1) || (not . isInside) (x1, y1' - 1))
            )
            hors
    v2Intersects =
        filter
            ( \((x1', y1'), (x2', _)) ->
                x2 >= min x1' x2'
                    && x2 <= max x1' x2'
                    && minY < y1'
                    && maxY > y1'
                    && ((not . isInside) (x2, y1' + 1) || (not . isInside) (x2, y1' - 1))
            )
            hors

    tilesInRect =
        all
            ( all isInside
                . (\(x', y') -> [(x' + 1, y' + 1), (x' - 1, y' - 1), (x' + 1, y' - 1), (x' - 1, y' + 1)])
            )
            . filter (\(x', y') -> x' > minX && x' < maxX && y' > minY && y' < maxY)
            $ ts

    isInside :: (Int, Int) -> Bool
    isInside (x, y) =
        (odd . length . filter (\((x1', y1'), (_, y2')) -> y > min y1' y2' && y < max y1' y2' && x < x1')) vers
            || any (\((x1', y1'), (x2', _)) -> y == y1' && x >= min x1' x2' && x <= max x1' x2') hors

part2Solution :: [(Int, Int)] -> Int
part2Solution ts =
    maximum
        [ (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)
        | i <- [0 .. length ts - 2]
        , let t1@(x1, y1) = ts !! i
        , j <- [i + 1 .. length ts - 1]
        , let t2@(x2, y2) = ts !! j
        , isGoodArea t1 t2 poly ts
        ]
  where
    poly = head . mkPolys $ ts
