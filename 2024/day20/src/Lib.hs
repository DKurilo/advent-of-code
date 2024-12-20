{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)

data Point = P
  { px :: Int
  , py :: Int
  } deriving (Eq, Ord, Show)

data Track = Track
  { tTrack :: S.Set Point
  , tWalls :: S.Set Point
  , tStart :: Point
  , tEnd :: Point
  , tMinSave :: Int
  } deriving (Show)

type TrackMap = M.Map Point Int

parse :: [String] -> Track
parse css = Track tr walls start end minSave
  where
    minSave = read . head $ css
    mp = tail css
    w = length . head $ mp
    h = length mp
    traverseChars :: (Char -> Bool) -> [Point]
    traverseChars pr =
      [P x y | x <- [0 .. w - 1], y <- [0 .. h - 1], pr (mp !! y !! x)]
    start = head $ traverseChars (== 'S')
    end = head $ traverseChars (== 'E')
    tr = S.fromList . traverseChars $ (/= '#')
    walls = S.fromList . traverseChars $ (== '#')

heatMap :: Track -> TrackMap
heatMap tr = doer end end
  where
    end = M.singleton (tEnd tr) 0
    doer :: TrackMap -> TrackMap -> TrackMap
    doer front visited
      | M.null front = visited
      | otherwise = doer front' (M.unionWith min visited front')
      where
        front' =
          M.fromList
            . concatMap
                (\(p, x) ->
                   [ (p', x + 1)
                   | p' <-
                       [ p {py = py p - 1}
                       , p {px = px p + 1}
                       , p {py = py p + 1}
                       , p {px = px p - 1}
                       ]
                   , canGo p' (x + 1)
                   ])
            . M.toList
            $ front
        canGo :: Point -> Int -> Bool
        canGo p x =
          case p `M.lookup` visited of
            Just x'
              | x >= x' -> False
            _ -> p `S.member` tTrack tr

canGoInWall :: Int -> Track -> Point -> [(Point, Int)]
canGoInWall maxDist tr p0 = M.toList $ doer p0s p0s M.empty 1
  where
    p0s = S.singleton p0
    doer ::
         S.Set Point -> S.Set Point -> M.Map Point Int -> Int -> M.Map Point Int
    doer front visited endPoints dist
      | dist > maxDist = endPoints
      | S.null front = endPoints
      | otherwise =
        doer
          front'
          (visited `S.union` front')
          (M.unionWith min endPoints endPoints')
          (dist + 1)
      where
        possibleMoves =
          concatMap
            (\p ->
               [ p {py = py p - 1}
               , p {px = px p + 1}
               , p {py = py p + 1}
               , p {px = px p - 1}
               ])
            . S.toList
            $ front
        front' = S.fromList possibleMoves
            -- . filter (\p -> p `S.member` tWalls tr && p `S.notMember` visited)
            -- $ possibleMoves
        endPoints' =
          M.fromList . fmap (, dist) . filter (\p -> p `S.member` tTrack tr)
            $ possibleMoves

findJumps :: Track -> Int -> TrackMap -> [(Point, Point, Int, Int)]
findJumps tr maxJump tm =
  concatMap
    (\(p, x) ->
       [ (p, p', d, save)
       | (p', d) <- canGoInWall maxJump tr p
       , let x' = fromMaybe 100000 . M.lookup p' $ tm
       , let save = x - x' - d
       , save >= tMinSave tr
       ])
    . M.toList
    $ tm

part1Solution :: [String] -> Int
part1Solution css = length . findJumps tr 2 . heatMap $ tr
  where
    tr = parse css

part2Solution :: [String] -> Int
part2Solution css = length . findJumps tr 20 . heatMap $ tr
  where
    tr = parse css
