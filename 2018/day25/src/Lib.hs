module Lib
  ( part1Solution,
  )
where

import Data.List (foldl', intersect)
import Data.Map (Map (..))
import qualified Data.Map as M
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int, pZ :: Int, pW :: Int} deriving (Eq, Ord, Show)

instance Read Point where
  readPrec = do
    x <- readPrec
    lift . P.char $ ','
    y <- readPrec
    lift . P.char $ ','
    z <- readPrec
    lift . P.char $ ','
    Point x y z <$> readPrec

manhDist :: Point -> Point -> Int
manhDist p1 p2 = abs (pX p1 - pX p2) + abs (pY p1 - pY p2) + abs (pZ p1 - pZ p2) + abs (pW p1 - pW p2)

buildGraph :: [Point] -> Map Point [Point]
buildGraph =
  foldl'
    ( \g p ->
        M.mapWithKey (\p' ps -> if manhDist p p' <= 3 then p : ps else ps)
          . M.insert p (filter (\p' -> manhDist p' p <= 3) . M.keys $ g)
          $ g
    )
    M.empty

pullConstellations :: Map Point [Point] -> [[Point]]
pullConstellations g
  | null g = []
  | otherwise = firstConstellation : pullConstellations g'
  where
    (firstConstellation, g') = pullConstellation g

pullConstellation :: Map Point [Point] -> ([Point], Map Point [Point])
pullConstellation g = doer [] [firstPoint] g
  where
    firstPoint = fst . M.findMin $ g
    doer :: [Point] -> [Point] -> Map Point [Point] -> ([Point], Map Point [Point])
    doer visited front g'
      | null front = (visited, g')
      | otherwise = doer (front ++ visited) front' . M.filterWithKey (\p _ -> p `notElem` front) $ g'
      where
        front' = M.keys . M.filter (not . null . intersect front) $ g'

constellations :: [Point] -> [[Point]]
constellations = pullConstellations . buildGraph

part1Solution :: [Point] -> Int
part1Solution = length . constellations
