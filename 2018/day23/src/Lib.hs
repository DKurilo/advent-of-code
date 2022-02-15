module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Function (on)
import Data.List (maximumBy, minimumBy, nub, sortBy)
import Data.Ord (Ordering (..), compare)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int, pZ :: Int} deriving (Eq, Show)

instance Read Point where
  readPrec = do
    x <- readPrec
    lift . P.char $ ','
    y <- readPrec
    lift . P.char $ ','
    Point x y <$> readPrec

data NanoBot = NanoBot {nbP :: Point, nbR :: Int} deriving (Eq, Show)

instance Read NanoBot where
  readPrec = do
    lift . P.string $ "pos=<"
    p <- readPrec
    lift . P.string $ ">, r="
    NanoBot p <$> readPrec

manhattanDistance :: Point -> Point -> Int
manhattanDistance nb1 nb2 = abs (pX nb1 - pX nb2) + abs (pY nb1 - pY nb2) + abs (pZ nb1 - pZ nb2)

isInRange :: Point -> NanoBot -> Bool
isInRange p nb = manhattanDistance p (nbP nb) <= nbR nb

inRangeOf :: Point -> [NanoBot] -> [NanoBot]
inRangeOf p = filter (isInRange p)

inRangeOfCount :: Point -> [NanoBot] -> Int
inRangeOfCount p = length . inRangeOf p

pointsAround :: Int -> Point -> [Point]
pointsAround r p = [p {pX = x + r}, p {pX = x - r}, p {pY = y + r}, p {pY = y - r}, p {pZ = z + r}, p {pZ = z - r}]
  where
    x = pX p
    y = pY p
    z = pZ p

pointsAroundDiag :: Int -> Point -> [Point]
pointsAroundDiag r p =
  [ p {pX = pX p + dx, pY = pY p + dy, pZ = pZ p + dz}
    | dx <- [- r, 0, r],
      dy <- [- r, 0, r],
      dz <- [- r, 0, r],
      dx /= 0 || dy /= 0 || dz /= 0
  ]

vertices :: NanoBot -> [Point]
vertices nb = pointsAround (nbR nb) (nbP nb)

nbCmp :: (Point, Int) -> (Point, Int) -> Ordering
x1@(p1, n1) `nbCmp` x2@(p2, n2)
  | n1 > n2 = GT
  | n1 < n2 = LT
  | otherwise = manhattanDistance (Point 0 0 0) p2 `compare` manhattanDistance (Point 0 0 0) p1

bestPoint :: Point -> [NanoBot] -> [(Point, Int)] -> Point
bestPoint zero nbs = fst . bestFromList . map (\pn -> doer pn (manhattanDistance zero . fst $ pn))
  where
    bestFromList = maximumBy nbCmp
    doer :: (Point, Int) -> Int -> (Point, Int)
    doer pn@(p, n) r
      | null pns && r <= 1 = pn
      | null pns = doer pn (r `div` 2)
      | otherwise = doer bestPN (manhattanDistance zero . fst $ bestPN)
      where
        pns = closestPoints pn
        bestPN = bestFromList pns
        closestPoints (p, n) =
          filter
            (\(p', n') -> n' > n || n' == n && manhattanDistance zero p' < manhattanDistance zero p)
            . map (\p' -> (p', inRangeOfCount p' nbs))
            . pointsAroundDiag r
            $ p

part1Solution :: [NanoBot] -> Int
part1Solution nbs = length . filter ((`isInRange` strongestNb) . nbP) $ nbs
  where
    strongestNb = maximumBy (compare `on` nbR) nbs

part2Solution :: [NanoBot] -> Int
part2Solution nbs =
  manhattanDistance (Point 0 0 0)
    . bestPoint (Point 0 0 0) nbs
    $ bestVertices
  where
    allVertices = map (\p -> (p, p `inRangeOfCount` nbs)) . concatMap vertices $ nbs
    (_, mostInRange) = maximumBy nbCmp allVertices
    bestVertices = filter ((== mostInRange) . snd) allVertices
