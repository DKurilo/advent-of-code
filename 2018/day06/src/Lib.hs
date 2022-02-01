module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (nub)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = P {px :: Int, py :: Int} deriving (Eq, Ord, Show)

instance Read Point where
  readPrec = do
    x <- readPrec
    lift . P.char $ ','
    lift P.skipSpaces
    P x <$> readPrec

dist :: Point -> Point -> Int
dist p1 p2 = abs (px p1 - px p2) + abs (py p1 - py p2)

closest :: Point -> [Point] -> [Point]
closest p ps = map snd . filter ((== minD) . fst) $ ds
  where
    ds = map (\p1 -> (dist p p1, p1)) ps
    minD = minimum . map fst $ ds

borderPoints :: [(Point, Point)] -> Int -> Int -> Int -> Int -> [Point]
borderPoints ps minX maxX minY maxY =
  nub
    . map snd
    . filter (\(p, _) -> px p == minX || px p == maxX || py p == minY || py p == maxY)
    $ ps

part1Solution :: [Point] -> Int
part1Solution ps = maximum . map (\p -> length . filter (== p) $ innerCps) $ innerPs
  where
    xs = map px ps
    ys = map py ps
    minX = minimum xs
    maxX = maximum xs
    minY = minimum ys
    maxY = maximum ys
    cps = [(p, head cps) | x <- [minX .. maxX], y <- [minY .. maxY], let p = P x y, let cps = closest p ps, length cps == 1]
    bps = borderPoints cps minX maxX minY maxY
    innerPs = filter (`notElem` bps) ps
    innerCps = map snd . filter (\(p, cp) -> cp `elem` innerPs) $ cps

fill :: Int -> Point -> [Point] -> [Point]
fill d p ps = doer [p] [p]
  where
    isInRange :: Point -> Bool
    isInRange p = (< d) . sum . map (dist p) $ ps
    doer :: [Point] -> [Point] -> [Point]
    doer filled front
      | null front' = filled
      | otherwise = doer filled' front'
      where
        (filled', front') = foldl expand (filled, []) front
        expand :: ([Point], [Point]) -> Point -> ([Point], [Point])
        expand (filled'', front'') p' = (ps' ++ filled'', ps' ++ front'')
          where
            ps' =
              [ p''
                | dx <- [-1 .. 1],
                  dy <- [-1 .. 1],
                  dx /= 0 || dy /= 0,
                  let p'' = P (px p' + dx) (py p' + dy),
                  p'' `notElem` filled'',
                  isInRange p''
              ]

part2Solution :: [Point] -> Int
part2Solution ps = length . fill 10000 (P centerX centerY) $ ps
  where
    l = length ps
    centerX = (`div` l) . sum . map px $ ps
    centerY = (`div` l) . sum . map py $ ps
