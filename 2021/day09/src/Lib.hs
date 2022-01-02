module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (nub, product, sort, sortBy)
import Data.Vector (Vector (..), (!))
import qualified Data.Vector as V

data Point = P Int Int deriving (Eq, Ord, Show)

isLower :: Point -> Vector (Vector Int) -> Bool
isLower (P x y) hm = up && left && down && right
  where
    l = V.length hm - 1
    m = V.length (hm ! y) - 1
    up = y > 0 && hm ! (y - 1) ! x > hm ! y ! x || y == 0
    left = x > 0 && hm ! y ! (x - 1) > hm ! y ! x || x == 0
    down = y < l && hm ! (y + 1) ! x > hm ! y ! x || y == l
    right = x < m && hm ! y ! (x + 1) > hm ! y ! x || x == m

getLowers :: Vector (Vector Int) -> [Point]
getLowers hm =
  map snd . filter fst $
    [ (isLower (P x y) hm, P x y)
      | y <- [0 .. (V.length hm - 1)],
        x <- [0 .. (V.length (hm ! 0) - 1)]
    ]

getBasin :: Vector (Vector Int) -> Point -> [Point]
getBasin = doer []
  where
    doer :: [Point] -> Vector (Vector Int) -> Point -> [Point]
    doer basin hm p@(P x y)
      | y < 0 || y >= l = basin
      | x < 0 || x >= m = basin
      | hm ! y ! x == 9 = basin
      | p `elem` basin = basin
      | otherwise = right
      where
        l = V.length hm
        m = V.length (hm ! y)
        basin' = P x y : basin
        up = doer basin' hm (P x (y - 1))
        left = doer up hm (P (x - 1) y)
        down = doer left hm (P x (y + 1))
        right = doer down hm (P (x + 1) y)

part1Solution :: Vector (Vector Int) -> Int
part1Solution hm = sum . map (\(P x y) -> hm ! y ! x + 1) . getLowers $ hm

part2Solution :: Vector (Vector Int) -> Int
part2Solution hm =
  product
    . map length
    . take 3
    . sortBy (\ps1 ps2 -> compare (length ps2) (length ps1))
    . nub
    . map (sort . nub . getBasin hm)
    . getLowers
    $ hm
