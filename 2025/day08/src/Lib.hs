module Lib (
    part1Solution,
    part2Solution,
    parseJ,
)
where

import Data.List (scanl', sortOn)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Debug.Trace (trace)

data J = J {jx :: !Integer, jy :: !Integer, jz :: !Integer} deriving (Eq, Show)

newtype NS = NS {unNs :: [(J, Int)]} deriving (Eq, Show)

dist :: J -> J -> Integer
dist (J x1 y1 z1) (J x2 y2 z2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)

parseJ :: String -> J
parseJ cs = J x y z
  where
    (x : y : z : _) = fmap read . splitOn "," $ cs

findId :: J -> NS -> Int
findId j = snd . head . filter ((== j) . fst) . unNs

joinNs :: NS -> J -> J -> NS
joinNs ns j1 j2 = NS . fmap (\(j, n) -> (j, if n == n2Id' then n1Id' else n)) . unNs $ ns
  where
    n1Id = findId j1 ns
    n2Id = findId j2 ns
    n1Id' = min n1Id n2Id
    n2Id' = max n1Id n2Id

part1Solution :: [J] -> Int
part1Solution js =
    product
        . take 3
        . sortOn (\x -> -x)
        . M.elems
        . foldl' (\jsPerNs (_, x) -> M.insertWith (+) x 1 jsPerNs) M.empty
        . unNs
        $ ns1
  where
    ds =
        sortOn
            (\(_, _, d) -> d)
            [ (j1, j2, dist j1 j2)
            | i <- [0 .. length js - 2]
            , let j1 = js !! i
            , j <- [i + 1 .. length js - 1]
            , let j2 = js !! j
            ]
    ns = NS . zipWith (\n j -> (j, n)) [0 ..] $ js
    ns1 = foldl' (\ns' (j1, j2, _) -> joinNs ns' j1 j2) ns . take 1000 $ ds

part2Solution :: [J] -> Integer
part2Solution js = jx j1 * jx j2
  where
    ds =
        sortOn
            (\(_, _, d) -> d)
            [ (j1, j2, dist j1 j2)
            | i <- [0 .. length js - 2]
            , let j1 = js !! i
            , j <- [i + 1 .. length js - 1]
            , let j2 = js !! j
            ]
    ns = NS . zipWith (\n j -> (j, n)) [0 ..] $ js
    (ns1, (j1, j2)) =
        head
            . dropWhile (any ((/= 0) . snd) . unNs . fst)
            . scanl' (\(ns', _) (j1, j2, _) -> (joinNs ns' j1 j2, (j1, j2))) (ns, (J 0 0 0, J 0 0 0))
            $ ds
