module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.List (foldl', sortOn)
import qualified Data.Map as M
import qualified Data.Set as S

data Point = P
  { px :: Int
  , py :: Int
  } deriving (Eq, Ord, Show)

type Region = (S.Set Point, Char)

buildRegions :: [String] -> [Region]
buildRegions css = snd . foldl' doer (S.empty, []) $ garden
  where
    garden =
      [ (P x y, c)
      | x <- [0 .. (length . head) css - 1]
      , y <- [0 .. length css - 1]
      , let c = css !! y !! x
      ]
    gardenMap = M.fromList garden
    doer :: (S.Set Point, [Region]) -> (Point, Char) -> (S.Set Point, [Region])
    doer (visited, regs) (p, c)
      | p `S.member` visited = (visited, regs)
      | otherwise = (visited `S.union` reg, (reg, c) : regs)
      where
        reg = traverseRegion p c S.empty
    traverseRegion :: Point -> Char -> S.Set Point -> S.Set Point
    traverseRegion p c reg
      | p `S.member` reg = reg
      | otherwise =
        case p `M.lookup` gardenMap of
          Just c'
            | c' == c ->
              traverseRegion (p {py = py p + 1}) c
                . traverseRegion (p {py = py p - 1}) c
                . traverseRegion (p {px = px p - 1}) c
                . traverseRegion (p {px = px p + 1}) c
                . traverseRegion (p {px = px p + 1}) c
                . S.insert p
                $ reg
          _ -> reg

area :: Region -> Int
area = S.size . fst

perimeter :: Region -> Int
perimeter (reg, _) =
  sum
    [ u + r + d + l
    | p <- S.toList reg
    , let u = val (p {py = py p - 1})
    , let r = val (p {px = px p + 1})
    , let d = val (p {py = py p + 1})
    , let l = val (p {px = px p - 1})
    ]
  where
    val :: Point -> Int
    val p
      | p `S.member` reg = 0
      | otherwise = 1

sides :: Region -> Int
sides (reg, _) = sds py px pus + sds px py prs + sds py px pds + sds px py pls
  where
    pdirs :: Int -> Int -> [Point]
    pdirs dx dy =
      S.toList
        . S.filter
            (\p -> (p {px = px p + dx, py = py p + dy}) `S.notMember` reg)
        $ reg
    pus = sortOn (\p -> (py p, px p)) $ pdirs 0 (-1)
    prs = sortOn (\p -> (px p, py p)) $ pdirs 1 0
    pds = sortOn (\p -> (py p, px p)) $ pdirs 0 1
    pls = sortOn (\p -> (px p, py p)) $ pdirs (-1) 0
    sds :: (Point -> Int) -> (Point -> Int) -> [Point] -> Int
    sds _ _ [] = 0
    sds level next (p:ps) =
      (\(_, _, x) -> x)
        . foldl'
            (\(l, n, x) p' ->
               if level p' == l && abs (next p' - n) == 1
                 then (l, next p', x)
                 else (level p', next p', x + 1))
            (level p, next p, 1)
        $ ps

part1Solution :: [String] -> Int
part1Solution = sum . fmap (\reg -> area reg * perimeter reg) . buildRegions

part2Solution :: [String] -> Int
part2Solution = sum . fmap (\reg -> area reg * sides reg) . buildRegions
