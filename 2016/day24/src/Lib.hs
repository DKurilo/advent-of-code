module Lib
  ( part1Solution,
    part2Solution,
    mkAirDuct,
  )
where

import Data.List (permutations)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data AirDuct = AirDuct {adMap :: [String], ad0 :: (Int, Int), adPois :: [(Char, (Int, Int))]} deriving (Show)

mkAirDuct :: String -> AirDuct
mkAirDuct cs = AirDuct css startPoint pois
  where
    css = lines cs
    (startPoint, pois) =
      foldl
        ( \(sp, pois') (x, y) -> case css !! y !! x of
            '#' -> (sp, pois')
            '.' -> (sp, pois')
            '0' -> ((x, y), pois')
            c -> (sp, (c, (x, y)) : pois')
        )
        ((0, 0), [])
        [(x, y) | y <- [0 .. length css - 1], x <- [0 .. length (css !! y) - 1]]

getDistances :: AirDuct -> Char -> (Int, Int) -> M.Map (Char, Char) Int
getDistances ad c startP = doer 0 (S.singleton startP) (S.singleton startP)
  where
    adm = adMap ad
    adh = length adm
    adw = length . head $ adm
    doer :: Int -> S.Set (Int, Int) -> S.Set (Int, Int) -> M.Map (Char, Char) Int
    doer step visited front
      | S.null front = M.empty
      | otherwise = found `M.union` doer (step + 1) visited' front'
      where
        steps =
          concatMap
            ( \(x, y) ->
                [ (c', (x', y'))
                  | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
                    let x' = x + dx,
                    x' >= 0 && x' < adw,
                    let y' = y + dy,
                    y' >= 0 && y' < adh,
                    let c' = adm !! y' !! x',
                    c' /= '#',
                    (x', y') `S.notMember` visited
                ]
            )
            . S.toList
            $ front
        visited' = visited `S.union` (S.fromList . map snd) steps
        front' = S.fromList . map snd $ steps
        found = M.fromList . map (\c' -> ((c, c'), step + 1)) . filter (\c' -> c' /= '.' && c' > c) . map fst $ steps

allDistances :: AirDuct -> M.Map (Char, Char) Int
allDistances ad = M.unions . map (uncurry (getDistances ad)) . (('0', ad0 ad) :) . adPois $ ad

getDistance :: M.Map (Char, Char) Int -> String -> Int
getDistance dists cs = sum . zipWith (\c c' -> fromMaybe 100 . M.lookup (min c c', max c c') $ dists) cs . tail $ cs

findMinDistance :: (String -> String) -> AirDuct -> Int
findMinDistance f ad = minimum . map (getDistance dists . f) . permutations . map fst . adPois $ ad
  where
    dists = allDistances ad

part1Solution :: AirDuct -> Int
part1Solution = findMinDistance ('0' :)

part2Solution :: AirDuct -> Int
part2Solution = findMinDistance (\cs -> ('0' : cs) ++ "0")
