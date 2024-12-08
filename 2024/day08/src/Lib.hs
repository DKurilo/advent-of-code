module Lib
  ( part1Solution
  , part2Solution
  ) where

import qualified Data.Map as M
import qualified Data.Set as S

data Point = P
  { px :: Int
  , py :: Int
  } deriving (Show, Eq, Ord)

data AMap = AMap
  { amW :: Int
  , amH :: Int
  , amM :: M.Map Char [Point]
  } deriving (Show)

parseAntennas :: [String] -> AMap
parseAntennas css =
  AMap w h . M.fromListWith (<>)
    $ [ (c, [P x y])
      | x <- [0 .. w - 1]
      , y <- [0 .. h - 1]
      , let c = css !! y !! x
      , c /= '.'
      ]
  where
    w = length . head $ css
    h = length css

kFromN :: Int -> [a] -> [[a]]
kFromN 0 _ = [[]]
kFromN k ns =
  concat
    [ fmap (ns !! i :) . kFromN (k - 1) . drop (i + 1) $ ns
    | i <- [0 .. length ns - k]
    ]

isInside :: Point -> Point -> Point -> Bool
isInside tl br p =
  px p >= px tl && px p <= px br && py p >= py tl && py p <= py br

antinodes1 :: Point -> Point -> Point -> Point -> [Point]
antinodes1 tl br p1 p2 =
  filter (isInside tl br)
    $ [P (px p1 + dx) (py p1 + dy), P (px p2 - dx) (py p2 - dy)]
        <> [ P (px p1 - dx `div` 3) (py p1 - dy `div` 3)
           | dx `mod` 3 == 0 && dy `mod` 3 == 0
           ]
        <> [ P (px p1 - 2 * (dx `div` 3)) (py p1 - 2 * (dy `div` 3))
           | dx `mod` 3 == 0 && dy `mod` 3 == 0
           ]
  where
    dx = px p1 - px p2
    dy = py p1 - py p2

findAntinodes :: (Point -> Point -> [Point]) -> [Point] -> S.Set Point
findAntinodes nodeGetter =
  S.fromList . concatMap (\ps -> nodeGetter (head ps) (ps !! 1)) . kFromN 2

solution :: (Point -> Point -> Point -> Point -> [Point]) -> [String] -> Int
solution antiNodesGetter css =
  S.size
    . S.unions
    . fmap (findAntinodes nodeGetter)
    . filter ((>= 2) . length)
    . M.elems
    . amM
    $ am
  where
    am = parseAntennas css
    nodeGetter = antiNodesGetter (P 0 0) (P (amW am - 1) (amH am - 1))

part1Solution :: [String] -> Int
part1Solution = solution antinodes1

antinodes2 :: Point -> Point -> Point -> Point -> [Point]
antinodes2 tl br p1 p2 = [p1] <> psDir (+) <> psDir (-)
  where
    dx = px p1 - px p2
    dy = py p1 - py p2
    psDir :: (Int -> Int -> Int) -> [Point]
    psDir op =
      takeWhile (isInside tl br)
        . iterate (\p -> P (px p `op` dx) (py p `op` dy))
        $ p1

part2Solution :: [String] -> Int
part2Solution = solution antinodes2
