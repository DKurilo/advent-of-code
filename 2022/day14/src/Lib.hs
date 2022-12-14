module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

instance Read Point where
  readPrec = do
    x <- readPrec
    _ <- lift . P.char $ ','
    Point x <$> readPrec

newtype Path = Path {unPath :: [Point]} deriving (Eq, Show)

instance Read Path where
  readPrec = fmap Path . lift . P.many1 $ do
    p <- PC.readPrec_to_P readPrec 0
    _ <- P.optional . P.string $ " -> "
    return p

data Tile = Rock | Sand deriving (Eq, Show)

data Cave = Cave
  { cMap :: M.Map Point Tile,
    cLeft :: Int,
    cRight :: Int,
    cDown :: Int,
    cSource :: Point
  }
  deriving (Show)

addPathLineToCaveMap :: M.Map Point Tile -> (Point, Point) -> M.Map Point Tile
addPathLineToCaveMap m (pFrom, pTo)
  | pX pFrom == pX pTo = foldl' (\m' y -> M.insert (Point (pX pFrom) y) Rock m') m [yMin .. yMax]
  | pY pFrom == pY pTo = foldl' (\m' x -> M.insert (Point x (pY pFrom)) Rock m') m [xMin .. xMax]
  | otherwise = m
  where
    xMin = min (pX pFrom) (pX pTo)
    xMax = max (pX pFrom) (pX pTo)
    yMin = min (pY pFrom) (pY pTo)
    yMax = max (pY pFrom) (pY pTo)

addPathToCaveMap :: M.Map Point Tile -> Path -> M.Map Point Tile
addPathToCaveMap m (Path path) = foldl' addPathLineToCaveMap m plines
  where
    plines = zip path . tail $ path

pathesToCave :: [Path] -> Cave
pathesToCave pathes = Cave {cMap = m, cLeft = xMin, cRight = xMax, cDown = yMax, cSource = Point {pX = 500, pY = 0}}
  where
    m = foldl' addPathToCaveMap M.empty pathes
    ps = M.keys m
    xs = map pX ps
    xMin = minimum xs
    xMax = maximum xs
    ys = map pY ps
    yMax = maximum ys

pushSand1 :: Point -> Cave -> Maybe Cave
pushSand1 p cave
  | pX p < cLeft cave = Nothing
  | pX p > cRight cave = Nothing
  | pY p > cDown cave = Nothing
  | otherwise = case pD `M.lookup` cMap cave of
    Nothing -> pushSand1 pD cave
    _ -> case pDL `M.lookup` cMap cave of
      Nothing -> pushSand1 pDL cave
      _ -> case pDR `M.lookup` cMap cave of
        Nothing -> pushSand1 pDR cave
        _ -> Just $ cave {cMap = M.insert p Sand . cMap $ cave}
  where
    pD = p {pY = pY p + 1}
    pDL = pD {pX = pX pD - 1}
    pDR = pD {pX = pX pD + 1}

pushSand2 :: Point -> Cave -> Maybe Cave
pushSand2 p cave
  | p == cSource cave && cantMove = Nothing
  | pY p == 1 + cDown cave = Just $ cave {cMap = M.insert p Sand . cMap $ cave}
  | otherwise = case pD `M.lookup` cMap cave of
    Nothing -> pushSand2 pD cave
    _ -> case pDL `M.lookup` cMap cave of
      Nothing -> pushSand2 pDL cave
      _ -> case pDR `M.lookup` cMap cave of
        Nothing -> pushSand2 pDR cave
        _ -> Just $ cave {cMap = M.insert p Sand . cMap $ cave}
  where
    pD = p {pY = pY p + 1}
    pDL = pD {pX = pX pD - 1}
    pDR = pD {pX = pX pD + 1}
    cantMove =
      (isJust . M.lookup pD . cMap) cave
        && (isJust . M.lookup pDL . cMap) cave
        && (isJust . M.lookup pDR . cMap) cave

turn :: (Point -> Cave -> Maybe Cave) -> Maybe Cave -> Maybe Cave
turn _ Nothing = Nothing
turn pushSand (Just cave) = pushSand (cSource cave) cave

simulate :: (Point -> Cave -> Maybe Cave) -> Cave -> Cave
simulate pushSand cave = fromMaybe cave . last . takeWhile isJust . iterate (turn pushSand) . Just $ cave

part1Solution :: [Path] -> Int
part1Solution = M.size . M.filter (== Sand) . cMap . simulate pushSand1 . pathesToCave

part2Solution :: [Path] -> Int
part2Solution = (+ 1) . M.size . M.filter (== Sand) . cMap . simulate pushSand2 . pathesToCave
