{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data Point = P
  { px :: Int
  , py :: Int
  } deriving (Eq, Ord, Show)

parseInput :: [String] -> M.Map Point Int
parseInput css =
  M.fromList
    [ (p, read [h])
    | x <- [0 .. (length . head) css - 1]
    , y <- [0 .. length css - 1]
    , let p = P x y
    , let h = css !! y !! x
    ]

possibleEntrances :: M.Map Point Int -> [Point]
possibleEntrances = M.keys . M.filter (== 0)

trailScore :: M.Map Point Int -> Point -> Int
trailScore tm startP =
  M.size . M.filter (== 9) . doer (S.singleton startP) (startH + 1) $ start
  where
    start = M.filterWithKey (\p' _ -> p' == startP) tm
    startH = fromMaybe 0 . M.lookup startP $ start
    doer :: S.Set Point -> Int -> M.Map Point Int -> M.Map Point Int
    doer _ 10 visited = visited
    doer front h visited
      | S.null front = visited
      | otherwise = doer front' (h + 1) visited'
      where
        (front', visited') = foldl' (stepDoer h) (S.empty, visited) front
    stepDoer ::
         Int
      -> (S.Set Point, M.Map Point Int)
      -> Point
      -> (S.Set Point, M.Map Point Int)
    stepDoer h (front', visited') p' =
      (front' `S.union` ps, visited' `M.union` M.fromSet (const h) ps)
      where
        ps =
          S.fromList . filter (\p'' -> p'' `M.lookup` tm == Just h)
            $ [ p' {px = px p' + 1}
              , p' {px = px p' - 1}
              , p' {py = py p' + 1}
              , p' {py = py p' - 1}
              ]

part1Solution :: [String] -> Int
part1Solution css = sum . fmap (trailScore tm) . possibleEntrances $ tm
  where
    tm = parseInput css

trailRating :: M.Map Point Int -> Point -> Int
trailRating tm startP =
  sum
    . M.elems
    . M.map snd
    . M.filter ((== 9) . fst)
    . doer (M.singleton startP 1) (startH + 1)
    . M.map (, 1)
    $ start
  where
    start = M.filterWithKey (\p' _ -> p' == startP) tm
    startH = fromMaybe 0 . M.lookup startP $ start
    doer ::
         M.Map Point Int
      -> Int
      -> M.Map Point (Int, Int)
      -> M.Map Point (Int, Int)
    doer _ 10 visited = visited
    doer front h visited
      | M.null front = visited
      | otherwise = doer front' (h + 1) visited'
      where
        (front', visited') =
          foldl' (stepDoer h) (M.empty, visited) . M.toList $ front
    stepDoer ::
         Int
      -> (M.Map Point Int, M.Map Point (Int, Int))
      -> (Point, Int)
      -> (M.Map Point Int, M.Map Point (Int, Int))
    stepDoer h (front', visited') (p', ways) = (front'', visited'')
      where
        ps =
          S.fromList . filter (\p'' -> p'' `M.lookup` tm == Just h)
            $ [ p' {px = px p' + 1}
              , p' {px = px p' - 1}
              , p' {py = py p' + 1}
              , p' {py = py p' - 1}
              ]
        visited'' =
          M.unionWith (\(h', ways1) (_, ways2) -> (h', ways1 + ways2)) visited'
            . M.fromSet (const (h, ways))
            $ ps
        front'' = M.unionWith (+) front' . M.fromSet (const ways) $ ps

part2Solution :: [String] -> Int
part2Solution css = sum . fmap (trailRating tm) . possibleEntrances $ tm
  where
    tm = parseInput css
