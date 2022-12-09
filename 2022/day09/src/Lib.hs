module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', nub)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Show)

data Direction = L | R | U | D deriving (Eq, Show)

instance Read Direction where
  readPrec = do
    c <- lift P.get
    return $ case c of
      'L' -> L
      'R' -> R
      'U' -> U
      'D' -> D
      _ -> L

data Move = Move Direction Int deriving (Eq, Show)

instance Read Move where
  readPrec = do
    d <- readPrec
    lift P.skipSpaces
    Move d <$> readPrec

data Rope = Rope {rHead :: Point, rTail :: [Point], rVisited :: [Point]} deriving (Show)

areAdjacentBy :: (Point -> Int) -> Point -> Point -> Bool
areAdjacentBy f p1 p2 = abs (f p1 - f p2) <= 1

areAdjacent :: Point -> Point -> Bool
areAdjacent p1 p2 = areAdjacentBy pX p1 p2 && areAdjacentBy pY p1 p2

move1 :: Rope -> Direction -> Rope
move1 r d = Rope h' (reverse ts') (head ts' : rVisited r)
  where
    h = rHead r
    h' = case d of
      L -> h {pX = pX h - 1}
      R -> h {pX = pX h + 1}
      U -> h {pY = pY h - 1}
      D -> h {pY = pY h + 1}
    ts = rTail r
    ts' = snd . foldl' moveNode (h', []) $ ts
    moveNode :: (Point, [Point]) -> Point -> (Point, [Point])
    moveNode (prev, ts'') t
      | areAdjacent prev t = (t, t : ts'')
      | otherwise = (t', t' : ts'')
      where
        t' = Point (pX t + signum (pX prev - pX t)) (pY t + signum (pY prev - pY t))

move :: Rope -> Move -> Rope
move r (Move d n) = foldl' (\r' _ -> move1 r' d) r [1 .. n]

part1Solution :: [Move] -> Int
part1Solution = length . nub . rVisited . foldl' move (Rope (Point 0 0) [Point 0 0] [Point 0 0])

part2Solution :: [Move] -> Int
part2Solution = length . nub . rVisited . foldl' move (Rope (Point 0 0) (replicate 9 $ Point 0 0) [Point 0 0])
