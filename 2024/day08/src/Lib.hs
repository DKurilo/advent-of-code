module Lib
  ( part1Solution
  , part2Solution
  ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

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
  filter
    (isInside tl br)
    [P (px p1 + dx) (py p1 + dy), P (px p2 - dx) (py p2 - dy)]
  where
    dx = px p1 - px p2
    dy = py p1 - py p2

findAntinodes :: (Point -> Point -> [Point]) -> [Point] -> S.Set Point
findAntinodes nodeGetter =
  S.fromList . concatMap (\ps -> nodeGetter (head ps) (ps !! 1)) . kFromN 2

part1Solution :: [String] -> Int
part1Solution css =
  S.size
    . S.unions
    . fmap (findAntinodes nodeGetter)
    . filter ((>= 2) . length)
    . M.elems
    . amM
    $ am
  where
    am = parseAntennas css
    nodeGetter = antinodes1 (P 0 0) (P (amW am - 1) (amH am - 1))

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
part2Solution css =
  S.size
    . S.unions
    . fmap (findAntinodes nodeGetter)
    . filter ((>= 2) . length)
    . M.elems
    . amM
    $ am
  where
    am = parseAntennas css
    nodeGetter = antinodes2 (P 0 0) (P (amW am - 1) (amH am - 1))
