module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', nub)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

instance Read Point where
  readPrec = do
    _ <- lift . P.string $ "x="
    x <- readPrec
    _ <- lift . P.string $ ", y="
    Point x <$> readPrec

manhattan :: Point -> Point -> Int
manhattan p1 p2 = abs (pX p1 - pX p2) + abs (pY p1 - pY p2)

data Sensor = Sensor {senMe :: Point, senBeacon :: Point} deriving (Eq, Show)

instance Read Sensor where
  readPrec = do
    _ <- lift . P.string $ "Sensor at "
    me <- readPrec
    _ <- lift . P.string $ ": closest beacon is at "
    Sensor me <$> readPrec

data Segment = Segment {sFrom :: Int, sTo :: Int} deriving (Eq, Show)

mkSegment :: Int -> Int -> Segment
mkSegment x1 x2 = Segment (min x1 x2) (max x1 x2)

segmentLength :: Segment -> Int
segmentLength s = 1 + sTo s - sFrom s

isInside :: Int -> Segment -> Bool
isInside x s = x >= sFrom s && x <= sTo s

areOverlapped :: Segment -> Segment -> Bool
areOverlapped s1 s2 = isInside (sFrom s1) s2 || isInside (sTo s1) s2 || isInside (sFrom s2) s1 || isInside (sTo s2) s1

areAdjacent :: Segment -> Segment -> Bool
areAdjacent s1 s2 = sTo s1 == sFrom s2 - 1 || sTo s2 == sFrom s1 - 1

addSegment :: Segment -> [Segment] -> [Segment]
addSegment s = reverse . uncurry (:) . foldl' doer (s, [])
  where
    doer :: (Segment, [Segment]) -> Segment -> (Segment, [Segment])
    doer (s', segments) sToAdd
      | areOverlapped s' sToAdd || areAdjacent s' sToAdd =
        (mkSegment (min (sFrom s') (sFrom sToAdd)) (max (sTo s') (sTo sToAdd)), segments)
      | sFrom sToAdd > sTo s' + 1 = (sToAdd, s' : segments)
      | sFrom s' > sTo sToAdd + 1 = (s', sToAdd : segments)
      | otherwise = error "something is wrong"

coverOnYBySensor :: Int -> Sensor -> Maybe Segment
coverOnYBySensor y sensor
  | dist <= 0 = Nothing
  | otherwise = Just $ Segment ((pX . senMe) sensor - dist) ((pX . senMe) sensor + dist)
  where
    dist = manhattan (senMe sensor) (senBeacon sensor) - abs (y - (pY . senMe) sensor)

coverOnY :: Int -> [Sensor] -> [Segment]
coverOnY n =
  foldl' (flip addSegment) []
    . fromMaybe []
    . sequence
    . filter isJust
    . map (coverOnYBySensor n)

cutCover :: Int -> Int -> [Segment] -> [Segment]
cutCover _ _ [] = []
cutCover from to (segment : segments)
  | sTo segment < from = cutCover from to segments
  | sFrom segment >= from && sTo segment <= to = segment : cutCover from to segments
  | sFrom segment < from = cutCover from to (segment {sFrom = from} : segments)
  | sTo segment > to = segment {sTo = to} : cutCover from to segments
  | otherwise = segments

part1Solution :: Int -> [Sensor] -> Int
part1Solution n sensors = (sum . map segmentLength) cover - length beaconsInCover
  where
    cover = coverOnY n sensors
    beacons = nub . map senBeacon $ sensors
    beaconsInCover = filter (\b -> pY b == n && any (isInside (pX b)) cover) beacons

part2Solution :: Int -> Int -> [Sensor] -> Int
part2Solution from to sensors = x * 4000000 + y
  where
    x = (+ 1) . sTo . head $ cover
    (y, cover) =
      head
        . filter (\(_, cover') -> (not . null) cover' && length cover' > 1)
        . zip [0 ..]
        . map (cutCover from to . (`coverOnY` sensors))
        $ [from .. to]
