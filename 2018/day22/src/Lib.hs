module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (second))
import Data.List (foldl', intercalate, nub)
import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set (..))
import qualified Data.Set as S
import Data.Vector (Vector (..), (!))
import qualified Data.Vector as V
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

xMultpier = 16807

yMultiplier = 48271

erosionLevelMod = 20183

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

instance Read Point where
  readPrec = do
    x <- readPrec
    lift . P.char $ ','
    Point x <$> readPrec

data CaveDescription = CaveDescription {cDepth :: Int, cTarget :: Point} deriving (Eq, Ord, Show)

instance Read CaveDescription where
  readPrec = do
    lift . P.string $ "depth: "
    depth <- readPrec
    lift P.skipSpaces
    lift . P.string $ "target: "
    CaveDescription depth <$> readPrec

data CType = Rocky | Wet | Narrow deriving (Eq, Ord, Show)

riskLevel :: CType -> Int
riskLevel Rocky = 0
riskLevel Wet = 1
riskLevel Narrow = 2

ctypeToChar :: CType -> Char
ctypeToChar Rocky = '.'
ctypeToChar Wet = '='
ctypeToChar Narrow = '|'

erosionLevelToCType :: Int -> CType
erosionLevelToCType el = case el `mod` 3 of
  0 -> Rocky
  1 -> Wet
  _ -> Narrow

newtype Cave = Cave {unCave :: [[CType]]} deriving (Eq, Show)

mkCave :: CaveDescription -> Cave
mkCave cd = Cave . map (map erosionLevelToCType) $ erosionLevels
  where
    erosionLevels =
      [ [erosionLevel x y | x <- [0 ..]]
        | y <- [0 ..]
      ]
    erosionLevel :: Int -> Int -> Int
    erosionLevel 0 y = geologicalIndexToErosionLevel $ yMultiplier * y
    erosionLevel x 0 = geologicalIndexToErosionLevel $ xMultpier * x
    erosionLevel x y
      | x == (pX . cTarget) cd && y == (pY . cTarget) cd = geologicalIndexToErosionLevel 0
      | otherwise = geologicalIndexToErosionLevel $ erosionLevels !! (y - 1) !! x * erosionLevels !! y !! (x - 1)
    geologicalIndexToErosionLevel :: Int -> Int
    geologicalIndexToErosionLevel gi = (gi + cDepth cd) `mod` erosionLevelMod

showCave :: Point -> Point -> Cave -> String
showCave from to cv =
  intercalate
    "\n"
    [ [(ctypeToChar . (!! x) . (!! y) . unCave) cv | x <- [pX from .. pX to]]
      | y <- [pY from .. pY to]
    ]

riskFactor :: Point -> Point -> Cave -> Int
riskFactor from to cv =
  sum . concat $
    [ [(riskLevel . (!! x) . (!! y) . unCave) cv | x <- [pX from .. pX to]]
      | y <- [pY from .. pY to]
    ]

data Ammunition = Torch | ClimbingGear | Neither deriving (Eq, Show)

data Me = Me {meCoord :: Point, meAmmunition :: Ammunition} deriving (Eq, Show)

showMx :: Map Point Int -> String
showMx mx = intercalate "\n" [concat [(fmt . show . fromMaybe 0) (Point x y `M.lookup` mx) | x <- [minY .. maxY]] | y <- [minY .. maxY]]
  where
    ps = M.keys mx
    minX = minimum . map pX $ ps
    maxX = maximum . map pX $ ps
    minY = minimum . map pY $ ps
    maxY = maximum . map pY $ ps
    fmt cs = " " ++ (reverse . take 2 . reverse . ("00" ++)) cs ++ " "

manhDist :: Point -> Point -> Int
manhDist p1 p2 = abs (pX p1 - pX p2) + abs (pY p1 - pY p2)

fastestWayTime :: Me -> Me -> Cave -> Maybe Int
fastestWayTime from to (Cave cv) =
  M.lookup (meCoord to) mx
  where
    maxDist = manhDist (meCoord from) (meCoord to) * 8
    dx = abs ((pX . meCoord) from - (pX . meCoord) to)
    dy = abs ((pY . meCoord) from - (pY . meCoord) to)
    xmax = (maxDist - dy + dx) `div` 2 + 1
    ymax = (maxDist - dx + dy) `div` 2 + 1
    vcv = V.fromList . take ymax . map (V.fromList . take xmax) $ cv
    mx = doer (M.singleton (meCoord from) 0) (M.singleton (meCoord from) (0, [meAmmunition from]))
    doer :: Map Point Int -> Map Point (Int, [Ammunition]) -> Map Point Int
    doer visited front
      | foundAll = visited
      | otherwise = doer visited' front''
      where
        foundAll = case meCoord to `M.lookup` visited of
          Just time
            | (M.null . M.filter ((< time) . fst)) front -> True
          _ -> False

        front' =
          M.map (second nub)
            . M.unionsWith
              ( \ta1@(time1, am1) ta2@(time2, am2) ->
                  if time1 < time2
                    then ta1
                    else if time2 < time1 then ta2 else (time1, am1 ++ am2)
              )
            . map oneStep
            . M.toList
            $ front

        oneStep :: (Point, (Int, [Ammunition])) -> Map Point (Int, [Ammunition])
        oneStep (coord, (time, ammunition)) = M.fromList $ do
          (dx, dy) <- [(0, 1), (0, -1), (1, 0), (-1, 0)]
          let (x, y) = (pX coord + dx, pY coord + dy)
          guard $ x >= 0 && y >= 0
          let currentCType = vcv ! pY coord ! pX coord
              nextCType = vcv ! y ! x
              (time', ammunition') = case (currentCType, nextCType) of
                (Rocky, Rocky) -> (time + 1, ammunition)
                (Rocky, Wet)
                  | ClimbingGear `elem` ammunition -> (time + 1, [ClimbingGear])
                  | otherwise -> (time + 8, [ClimbingGear])
                (Rocky, Narrow)
                  | Torch `elem` ammunition -> (time + 1, [Torch])
                  | otherwise -> (time + 8, [Torch])
                (Wet, Rocky)
                  | ClimbingGear `elem` ammunition -> (time + 1, [ClimbingGear])
                  | otherwise -> (time + 8, [ClimbingGear])
                (Wet, Wet) -> (time + 1, ammunition)
                (Wet, Narrow)
                  | Neither `elem` ammunition -> (time + 1, [Neither])
                  | otherwise -> (time + 8, [Neither])
                (Narrow, Rocky)
                  | Torch `elem` ammunition -> (time + 1, [Torch])
                  | otherwise -> (time + 8, [Torch])
                (Narrow, Wet)
                  | Neither `elem` ammunition -> (time + 1, [Neither])
                  | otherwise -> (time + 8, [Neither])
                (Narrow, Narrow) -> (time + 1, ammunition)
              coord' = Point x y
              time'' = if coord' == meCoord to && meAmmunition to `notElem` ammunition' then time' + 7 else time'
          return (coord', (time'', ammunition'))

        visited' = foldl' addMeToVisited visited . M.toList $ front'

        addMeToVisited :: Map Point Int -> (Point, (Int, [Ammunition])) -> Map Point Int
        addMeToVisited visited'' (coord, (time, _)) = case coord `M.lookup` visited'' of
          Just time'
            | time >= time' -> visited''
          _ -> M.insert coord time visited''

        maxTimeFromClosestFront
          | M.null front' = 0
          | otherwise = minimum . map (\(coord, (time, _)) -> time + manhDist coord (meCoord to) * 8) . M.toList $ front'
        alreadyAchieved = fromMaybe (manhDist (meCoord from) (meCoord to) * 8) . M.lookup (meCoord to) $ visited'
        maxTimeFromClosest = min maxTimeFromClosestFront alreadyAchieved
        front''
          | M.null front' = front'
          | otherwise = M.filterWithKey (\coord (time, _) -> (time + manhDist coord (meCoord to)) <= maxTimeFromClosest) front'

part1Solution :: CaveDescription -> Int
part1Solution cd = riskFactor (Point 0 0) (cTarget cd) . mkCave $ cd

part2Solution :: CaveDescription -> Maybe Int
part2Solution cd = fastestWayTime (Me (Point 0 0) Torch) (Me (cTarget cd) Torch) . mkCave $ cd
