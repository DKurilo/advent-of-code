module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Function (on)
import Data.List (foldl', maximumBy, sortOn)
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Ord (compare)
import Data.Ratio
import Data.Set (Set (..), fromList, insert, size, toList)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

data DXDY = DXDY {dX :: Int, dY :: Int} deriving (Eq, Show)

instance Ord DXDY where
  r1 <= r2
    | r1 == r2 = True
    | dx1 == 0 && dy1 == 1 = True
    | dx2 == 0 && dy2 == 1 = False
    | dx1 > 0 && dy1 > 0 && (dx2 <= 0 || dy2 <= 0) = True
    | dx2 > 0 && dy2 > 0 && (dx1 <= 0 || dy1 <= 0) = False
    | dx1 > 0 && dy1 > 0 && dx2 > 0 && dy2 > 0 = (dx1 % dy1) < (dx2 % dy2)
    | dx1 == 1 && dy1 == 0 = True
    | dx2 == 1 && dy2 == 0 = False
    | dx1 > 0 && dy1 < 0 && (dx2 <= 0 || dy2 >= 0) = True
    | dx2 > 0 && dy2 < 0 && (dx1 <= 0 || dy1 >= 0) = False
    | dx1 > 0 && dy1 < 0 && dx2 > 0 && dy2 < 0 = (dy1 % dx1) > (dy2 % dx2)
    | dx1 == 0 && dy1 == -1 = True
    | dx2 == 0 && dy2 == -1 = False
    | dx1 < 0 && dy1 < 0 && (dx2 >= 0 || dy2 >= 0) = True
    | dx2 < 0 && dy2 < 0 && (dx1 >= 0 || dy1 >= 0) = False
    | dx1 < 0 && dy1 < 0 && dx2 < 0 && dy2 < 0 = (dx1 % dy1) < (dx2 % dy2)
    | dx1 == -1 && dy1 == 0 = True
    | dx2 == -1 && dy2 == 0 = False
    | dx1 < 0 && dy1 > 0 && (dx2 >= 0 || dy2 <= 0) = True
    | dx2 < 0 && dy2 > 0 && (dx1 >= 0 || dy1 <= 0) = False
    | dx1 < 0 && dy1 > 0 && dx2 < 0 && dy2 > 0 = (dy1 % dx1) > (dy2 % dx2)
    | otherwise = True
    where
      dx1 = dX r1
      dy1 = dY r1
      dx2 = dX r2
      dy2 = dY r2

data AsteroidsMap = AsteroidsMap {amAs :: Set Point, amWidth :: Int, amHeight :: Int} deriving (Show)

parse :: [String] -> AsteroidsMap
parse css = AsteroidsMap am ((length . head) css) (length css)
  where
    am =
      fromList
        . map fst
        . filter ((== '#') . snd)
        . concat
        . zipWith (\y -> map (\(x, c) -> (Point x y, c))) [0 ..]
        . map (zip [0 ..])
        $ css

observes :: Point -> AsteroidsMap -> Map DXDY [Point]
observes p = foldl' doer M.empty . S.filter (/= p) . amAs
  where
    doer :: Map DXDY [Point] -> Point -> Map DXDY [Point]
    doer as a = M.insertWith (++) r [a] as
      where
        -- weird thing here to make it sorting clockwise
        dx = pX a - pX p
        dy = pY p - pY a
        cd = gcd (abs dx) (abs dy)
        r = DXDY (dx `div` cd) (dy `div` cd)

distance :: Point -> Point -> (Int, Int)
distance p1 p2 = (dx, dy)
  where
    dx = abs (pX p2 - pX p1)
    dy = abs (pY p2 - pY p1)

destroyedAsteroids :: Point -> Map DXDY [Point] -> [Point]
destroyedAsteroids p = concatMap (map head) . takeWhile (not . null) . iterate nextLayer . map (sortOn (distance p)) . M.elems
  where
    nextLayer :: [[Point]] -> [[Point]]
    nextLayer = filter (not . null) . map tail

part1Solution :: [String] -> Int
part1Solution css = maximum . map (M.size . (`observes` am)) . toList . amAs $ am
  where
    am = parse css

part2Solution :: [String] -> Int
part2Solution css = (\p -> pX p * 100 + pY p) . (!! 199) . destroyedAsteroids station $ m
  where
    am = parse css
    (m, station) =
      maximumBy (compare `on` (M.size . fst))
        . map (\p -> (p `observes` am, p))
        . toList
        . amAs
        $ am
