{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', intercalate)
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, maybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

newtype Vein = Vein {unVein :: [Point]} deriving (Show)

instance Read Vein where
  readPrec =
    lift $
      ( do
          P.string "x="
          x <- PC.readPrec_to_P readPrec 0
          P.string ", y="
          yStart <- PC.readPrec_to_P readPrec 0
          P.string ".."
          yEnd <- PC.readPrec_to_P readPrec 0
          return $ Vein . map (Point x) $ [yStart .. yEnd]
      )
        P.<++ ( do
                  P.string "y="
                  y <- PC.readPrec_to_P readPrec 0
                  P.string ", x="
                  xStart <- PC.readPrec_to_P readPrec 0
                  P.string ".."
                  xEnd <- PC.readPrec_to_P readPrec 0
                  return $ Vein . map (`Point` y) $ [xStart .. xEnd]
              )

data Ground = Clay | WDown | WRightLeft | WRight | WLeft | WStale | Sand deriving (Eq, Show)

groundToChar :: Ground -> Char
groundToChar Clay = '#'
groundToChar WDown = 'v'
groundToChar WRightLeft = '*'
groundToChar WRight = '>'
groundToChar WLeft = '<'
groundToChar WStale = '~'
groundToChar _ = '.'

canBeFilled :: Ground -> Bool
canBeFilled Clay = False
canBeFilled WStale = False
canBeFilled _ = True

data GMap = GMap {gMinY :: Int, gMaxY :: Int, gMap :: Map Point Ground} deriving (Eq, Show)

showGMap :: GMap -> String
showGMap gm =
  intercalate
    "\n"
    [ [ (maybe '.' groundToChar . M.lookup (Point x y) . gMap) gm
        | x <- [minX .. maxX]
      ]
      | y <- [gMinY gm .. gMaxY gm]
    ]
  where
    ps = M.keys . gMap $ gm
    minX = minimum . map pX $ ps
    maxX = maximum . map pX $ ps

mkMap :: [Vein] -> GMap
mkMap veins = GMap minY maxY . M.fromList $ clay
  where
    clay = map (,Clay) . concatMap unVein $ veins
    ys = map (pY . fst) clay
    minY = minimum ys
    maxY = maximum ys

processPoint :: GMap -> Point -> GMap
processPoint gm p
  | g /= WDown && canBeFilled gDown = gm {gMap = M.insert p WDown . gMap $ gm}
  | g == WDown && (gDown == Clay || gDown == WStale) = gm {gMap = M.insert p WRightLeft . gMap $ gm}
  | g == WDown && gDown == Sand && pY pDown <= gMaxY gm = gm {gMap = M.insert pDown WDown . gMap $ gm}
  | g == WDown = gm
  | g == WRightLeft && gDown == Sand && pY pDown <= gMaxY gm = gm {gMap = M.insert pDown WDown . M.insert p WDown . gMap $ gm}
  | g == WRightLeft && gDown == Sand = gm {gMap = M.insert p WDown . gMap $ gm}
  | g == WRightLeft && canBeFilled gDown = gm
  | g == WRightLeft && (not . canBeFilled) gLeft && (not . canBeFilled) gRight = gm {gMap = M.insert p WStale . gMap $ gm}
  | g == WRightLeft && ((not . canBeFilled) gLeft || gLeft == WRight) = gm {gMap = M.insert p WRight . gMap $ gm}
  | g == WRightLeft && ((not . canBeFilled) gRight || gRight == WLeft) = gm {gMap = M.insert p WLeft . gMap $ gm}
  | g == WRightLeft && gRight == Sand && gLeft == Sand =
    gm {gMap = M.insert pRight WRightLeft . M.insert pLeft WRightLeft . gMap $ gm}
  | g == WRightLeft && gRight == Sand = gm {gMap = M.insert pRight WRightLeft . gMap $ gm}
  | g == WRightLeft && gLeft == Sand = gm {gMap = M.insert pLeft WRightLeft . gMap $ gm}
  | g == WLeft && gLeft == Sand = gm {gMap = M.insert pLeft WLeft . gMap $ gm}
  | g == WLeft && ((not . canBeFilled) gLeft || gLeft == WRight) = gm {gMap = M.insert p WStale . gMap $ gm}
  | g == WLeft && gLeft == WRightLeft = gm {gMap = M.insert pLeft WLeft . gMap $ gm}
  | g == WRight && gRight == Sand = gm {gMap = M.insert pRight WRight . gMap $ gm}
  | g == WRight && ((not . canBeFilled) gRight || gRight == WLeft) = gm {gMap = M.insert p WStale . gMap $ gm}
  | g == WRight && gRight == WRightLeft = gm {gMap = M.insert pRight WRight . gMap $ gm}
  | otherwise = gm
  where
    pDown = p {pY = pY p + 1}
    pUp = p {pY = pY p - 1}
    pLeft = p {pX = pX p - 1}
    pRight = p {pX = pX p + 1}
    gInPoint p' = fromMaybe Sand . M.lookup p' . gMap $ gm
    g = gInPoint p
    gDown = gInPoint pDown
    gUp = gInPoint pUp
    gLeft = gInPoint pLeft
    gRight = gInPoint pRight

fillFrom :: Int -> GMap -> GMap
fillFrom xSpring initGm = doer (initGm {gMap = M.insert p0 WDown . gMap $ initGm})
  where
    p0 = Point xSpring . gMinY $ initGm
    doer :: GMap -> GMap
    doer gm
      | gm == gm' = gm
      | otherwise = doer gm'
      where
        gm' = foldl' processPoint gm . M.keys . M.filter (\g -> g /= Clay && g /= WStale) . gMap $ gm

countWater :: GMap -> Int
countWater = M.size . M.filter (/= Clay) . gMap

countStaleWater :: GMap -> Int
countStaleWater = M.size . M.filter (== WStale) . gMap

fillVeins :: [Vein] -> GMap
fillVeins = fillFrom 500 . mkMap

part1Solution :: [Vein] -> Int
part1Solution = countWater . fillVeins

part2Solution :: [Vein] -> Int
part2Solution = countStaleWater . fillVeins
