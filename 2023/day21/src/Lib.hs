{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (join)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as S

data Point = P {px :: Int, py :: Int} deriving (Show, Eq, Ord)

near :: Point -> [Point]
near p =
  [ p {px = px p + 1},
    p {px = px p - 1},
    p {py = py p + 1},
    p {py = py p - 1}
  ]

data Labyrinth = Labyrinth {lmap :: M.Map Point (Maybe Int), lme :: Point, lmaxx :: Int, lmaxy :: Int} deriving (Show)

readLabyrinth :: [String] -> Labyrinth
readLabyrinth css = Labyrinth m me maxX maxY
  where
    tps =
      concat
        . zipWith (\y -> zipWith (\x -> (P x y,)) [0 ..]) [0 ..]
        $ css

    m =
      M.fromList
        . fmap ((,Nothing) . fst)
        . filter ((/= '#') . snd)
        $ tps

    ps = M.keys m
    maxX = maximum . fmap px $ ps
    maxY = maximum . fmap py $ ps

    me = fst . head . filter ((== 'S') . snd) $ tps

oneStep :: (Int -> Bool) -> Labyrinth -> Labyrinth
oneStep predicate l =
  l
    { lmap =
        M.unionWith
          (const (fmap (+ 1)))
          (M.fromSet (const Nothing) ps)
          (lmap l)
    }
  where
    ps =
      S.filter (\p -> (fmap predicate . join . M.lookup p . lmap) l == Just True)
        . S.fromList
        . concatMap near
        . M.keys
        . M.filter ((== Just True) . fmap predicate)
        . lmap
        $ l

fillLabyrinth :: Labyrinth -> Labyrinth
fillLabyrinth l = doer 1 sme sme $ l {lmap = M.insert me (Just 0) . lmap $ l}
  where
    me = lme l
    sme = S.singleton me

    doer :: Int -> S.Set Point -> S.Set Point -> Labyrinth -> Labyrinth
    doer steps front visited l'
      | null front = l'
      | otherwise = doer (steps + 1) front' visited' l''
      where
        front' =
          S.filter
            ( \p ->
                p `S.notMember` visited
                  && p `M.member` lmap l
                  && (isNothing . join . M.lookup p . lmap) l'
            )
            . S.fromList
            . concatMap near
            . S.toList
            $ front
        visited' = front' `S.union` visited
        l'' = l' {lmap = M.fromSet (const (Just steps)) front' `M.union` lmap l'}

part1Solution :: [String] -> Int
part1Solution css =
  M.size
    . M.filter
      ( \mbst ->
          isJust mbst
            && fmap (\st -> st <= steps && (not . predicate) st) mbst == Just True
      )
    . lmap
    $ l''
  where
    steps = read . head . words . head $ css
    l = readLabyrinth . tail $ css
    l' = fillLabyrinth l
    predicate
      | even steps = odd
      | otherwise = even
    l'' = oneStep predicate l'

-- this solution works only when there is empty central horizontal and vertical
-- lines
part2Solution :: [String] -> Int
part2Solution css = wholeLabyrinthSteps + restStepsVertices + restStepsEdges
  where
    steps = read . last . words . head $ css
    l = readLabyrinth . tail $ css
    l' = fillLabyrinth l
    mostDistant = maximum . fmap (fromMaybe (-1)) . M.elems . lmap $ l'
    evenPos = possiblePosNumber even l'
    oddPos = possiblePosNumber odd l'
    maxLabirynthNumber = 1 + (steps - (px . lme) l') `div` (lmaxx l' + 1)
    wholeLabyrinthNumber = (steps - mostDistant) `div` (lmaxx l' + 1)
    layers
      | even steps = [evenPos, oddPos]
      | otherwise = [oddPos, evenPos]
    wholeLabyrinthSteps =
      sum [4 * i * (layers !! (i `mod` 2)) | i <- [1 .. wholeLabyrinthNumber]]
        + if even steps
          then evenPos
          else oddPos
    lFromRight = fillLabyrinth $ l {lme = P (lmaxx l) ((py . lme) l)}
    lFromLeft = fillLabyrinth $ l {lme = P 0 ((py . lme) l)}
    lFromTop = fillLabyrinth $ l {lme = P ((px . lme) l) 0}
    lFromBottom = fillLabyrinth $ l {lme = P ((px . lme) l) (lmaxy l)}
    lFromBottomRight = fillLabyrinth $ l {lme = P (lmaxx l) (lmaxy l)}
    lFromBottomLeft = fillLabyrinth $ l {lme = P 0 (lmaxy l)}
    lFromTopLeft = fillLabyrinth $ l {lme = P 0 0}
    lFromTopRight = fillLabyrinth $ l {lme = P (lmaxx l) 0}
    restStepsVertices =
      sum
        [ (sum . fmap (\(m, l'') -> m * possiblePosNumber predicate' l''))
            [ (1, lFromRight),
              (1, lFromLeft),
              (1, lFromTop),
              (1, lFromBottom)
            ]
          | i <- [wholeLabyrinthNumber + 1 .. maxLabirynthNumber],
            let d = i * (lmaxx l + 1) - (px . lme) l,
            let predicate = [even, odd] !! ((steps + d) `mod` 2),
            let predicate' x = predicate x && x + d <= steps
        ]
    restStepsEdges =
      sum
        [ (sum . fmap (\(m, l'') -> m * possiblePosNumber predicate' l''))
            [ (i - 1, lFromBottomRight),
              (i - 1, lFromBottomLeft),
              (i - 1, lFromTopLeft),
              (i - 1, lFromTopRight)
            ]
          | i <- [wholeLabyrinthNumber + 1 .. maxLabirynthNumber + 1],
            let d = (i - 2) * (lmaxx l + 1) + (px . lme) l + (py . lme) l + 2,
            let predicate = [even, odd] !! ((steps + d) `mod` 2),
            let predicate' x = predicate x && x + d <= steps
        ]

    possiblePosNumber :: (Int -> Bool) -> Labyrinth -> Int
    possiblePosNumber predicate = M.size . M.filter (\mbst -> fmap predicate mbst == Just True) . lmap
