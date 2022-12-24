{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

manhattan :: Point -> Point -> Int
manhattan (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

data Blizzard = BUp | BRight | BDown | BLeft deriving (Eq, Show)

charToBlizzard :: Char -> Blizzard
charToBlizzard '^' = BUp
charToBlizzard '>' = BRight
charToBlizzard 'v' = BDown
charToBlizzard '<' = BLeft
charToBlizzard _ = BUp

data Valley = Valley
  { vMe :: Point,
    vExit :: Point,
    vBlizzards :: M.Map Point [Blizzard],
    vWidth :: Int,
    vHeight :: Int
  }
  deriving (Show)

parseValley :: [String] -> Valley
parseValley css = doer (init . tail $ css) 1 (Valley me exit M.empty w h)
  where
    h = length css - 2
    w = (\x -> x - 2) . length . head $ css
    me = Point (length . takeWhile (== '#') . head $ css) 0
    exit = Point (length . takeWhile (== '#') . last $ css) (length css - 1)
    doer :: [String] -> Int -> Valley -> Valley
    doer [] _ v = v
    doer (cs : css') j v = doer css' (j + 1) (v {vBlizzards = blizzards'})
      where
        blizzards = vBlizzards v
        blizzards' =
          foldl'
            ( \bls (i, c) ->
                if c == '.'
                  then bls
                  else M.insert (Point i j) [charToBlizzard c] bls
            )
            blizzards
            . zip [1 ..]
            . init
            . tail
            $ cs

moveBlizzard :: Int -> Int -> (Point, Blizzard) -> (Point, Blizzard)
moveBlizzard _ h (p, BUp)
  | pY p == 1 = (p {pY = h}, BUp)
  | otherwise = (p {pY = pY p - 1}, BUp)
moveBlizzard w _ (p, BRight)
  | pX p == w = (p {pX = 1}, BRight)
  | otherwise = (p {pX = pX p + 1}, BRight)
moveBlizzard _ h (p, BDown)
  | pY p == h = (p {pY = 1}, BDown)
  | otherwise = (p {pY = pY p + 1}, BDown)
moveBlizzard w _ (p, BLeft)
  | pX p == 1 = (p {pX = w}, BLeft)
  | otherwise = (p {pX = pX p - 1}, BLeft)

moveBlizzards :: Valley -> Valley
moveBlizzards v =
  v
    { vBlizzards =
        M.fromListWith (<>)
          . map
            ( \pb ->
                let (p, b) = moveBlizzard (vWidth v) (vHeight v) pb
                 in (p, [b])
            )
          . concatMap (\(p, bs) -> map (p,) bs)
          . M.toList
          . vBlizzards
          $ v
    }

-- I made some stupid error (`-` in line 21) so I added this code to debug it
-- This code is not needed to solve the problem
traceBackPath :: Point -> [S.Set Point] -> [Point]
traceBackPath p [] = [p]
traceBackPath p (front : fronts) = p : traceBackPath (head . filter ((<= 1) . manhattan p) . S.toList $ front) fronts

findShortestPathLength :: Valley -> (Int, ([Point], Valley))
findShortestPathLength v = doer 0 (S.singleton . vMe $ v) [] v
  where
    width = vWidth v
    height = vHeight v
    doer :: Int -> S.Set Point -> [S.Set Point] -> Valley -> (Int, ([Point], Valley))
    doer n front prevFronts v'
      | vExit v `elem` front = (n, (traceBackPath (vExit v) prevFronts, v' {vMe = vExit v, vExit = vMe v}))
      | otherwise = doer (n + 1) front' (front : prevFronts) (moveBlizzards v')
      where
        front' =
          S.fromList
            . filter
              (\p -> p == vMe v || p == vExit v || pX p > 0 && pX p <= width && pY p > 0 && pY p <= height && p `M.notMember` vBlizzards v')
            . concatMap
              (\p -> [p, p {pY = pY p - 1}, p {pX = pX p + 1}, p {pY = pY p + 1}, p {pX = pX p -1}])
            $ front

part1Solution :: [String] -> Int
part1Solution = fst . findShortestPathLength . moveBlizzards . parseValley

part2Solution :: [String] -> Int
part2Solution css = n1 + n2 + n3
  where
    v = moveBlizzards . parseValley $ css
    (n1, (_, v')) = findShortestPathLength v
    (n2, (_, v'')) = findShortestPathLength v'
    (n3, _) = findShortestPathLength v''
