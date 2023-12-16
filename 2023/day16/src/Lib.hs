{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (second)
import qualified Data.Map as M

data Point = P {px :: Int, py :: Int} deriving (Show, Eq, Ord)

data Tile = MirrorLeft | MirrorRight | SplitterHor | SplitterVer deriving (Eq)

instance Show Tile where
  show MirrorLeft = "\\"
  show MirrorRight = "/"
  show SplitterHor = "-"
  show SplitterVer = "|"

charToTile :: Char -> Tile
charToTile '\\' = MirrorLeft
charToTile '/' = MirrorRight
charToTile '-' = SplitterHor
charToTile '|' = SplitterVer
charToTile c = error (c : " shouldn't be there")

data Direction = DUp | DDown | DRight | DLeft deriving (Eq, Show)

data FloorTiles = FT {ftWidth :: Int, ftHeight :: Int, ftMap :: M.Map Point Tile} deriving (Show)

newtype FloorLight = FL {unFL :: M.Map Point [Direction]} deriving (Show)

moveInDirection :: Direction -> Point -> Point
moveInDirection DUp p = p {py = py p - 1}
moveInDirection DDown p = p {py = py p + 1}
moveInDirection DLeft p = p {px = px p - 1}
moveInDirection DRight p = p {px = px p + 1}

fillLight :: Point -> Direction -> FloorLight -> FloorTiles -> FloorLight
fillLight p d fl ft
  | px p < 0 || px p >= ftWidth ft || py p < 0 || py p >= ftHeight ft = fl
  | otherwise = case p `M.lookup` unFL fl of
      Just ds
        | d `elem` ds -> fl
        | otherwise -> next ds
      _ -> next []
  where
    next :: [Direction] -> FloorLight
    next ds = case p `M.lookup` ftMap ft of
      Just MirrorLeft
        | d == DUp -> fillLight (moveInDirection DLeft p) DLeft fl' ft
        | d == DDown -> fillLight (moveInDirection DRight p) DRight fl' ft
        | d == DLeft -> fillLight (moveInDirection DUp p) DUp fl' ft
        | d == DRight -> fillLight (moveInDirection DDown p) DDown fl' ft
      Just MirrorRight
        | d == DUp -> fillLight (moveInDirection DRight p) DRight fl' ft
        | d == DDown -> fillLight (moveInDirection DLeft p) DLeft fl' ft
        | d == DLeft -> fillLight (moveInDirection DDown p) DDown fl' ft
        | d == DRight -> fillLight (moveInDirection DUp p) DUp fl' ft
      Just SplitterHor
        | d == DUp || d == DDown ->
            fillLight
              (moveInDirection DLeft p)
              DLeft
              (fillLight (moveInDirection DRight p) DRight fl' ft)
              ft
      Just SplitterVer
        | d == DLeft || d == DRight ->
            fillLight
              (moveInDirection DUp p)
              DUp
              (fillLight (moveInDirection DDown p) DDown fl' ft)
              ft
      _ -> fillLight (moveInDirection d p) d fl' ft
      where
        fl' = fl {unFL = M.insert p (d : ds) . unFL $ fl}

parse :: [String] -> FloorTiles
parse css =
  FT (length . head $ css) (length css)
    . M.fromList
    . fmap (second charToTile)
    . concatMap (filter ((/= '.') . snd) . zipWith (\x (y, c) -> (P x y, c)) [0 ..])
    . zipWith (\y -> fmap (y,)) [0 ..]
    $ css

part1Solution :: [String] -> Int
part1Solution = M.size . unFL . fillLight (P 0 0) DRight (FL M.empty) . parse

part2Solution :: [String] -> Int
part2Solution css =
  maximum . fmap (M.size . unFL) $
    concatMap (\x -> [fillLight (P x 0) DDown fl ft, fillLight (P x maxY) DUp fl ft]) [0 .. maxX]
      <> concatMap (\y -> [fillLight (P 0 y) DRight fl ft, fillLight (P maxX y) DLeft fl ft]) [0 .. maxY]
  where
    fl = FL M.empty
    ft = parse css
    maxX = ftWidth ft - 1
    maxY = ftHeight ft - 1
