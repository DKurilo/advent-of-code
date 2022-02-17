module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Function (on)
import Data.List (foldl1, minimumBy)
import Data.Map (Map (..), empty, fromList, intersectionWith, keys, toList, unionWith)
import qualified Data.Map as M
import Data.Ord (compare)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Direction = R | D | L | U deriving (Eq, Show)

instance Read Direction where
  readPrec = lift $ (P.char 'R' >> return R) P.+++ (P.char 'D' >> return D) P.+++ (P.char 'L' >> return L) P.+++ (P.char 'U' >> return U)

data WirePart = WirePart {wpD :: Direction, wpL :: Int} deriving (Eq, Show)

instance Read WirePart where
  readPrec = do
    d <- readPrec
    WirePart d <$> readPrec

newtype Wire = Wire {unWire :: [WirePart]} deriving (Eq, Show)

instance Read Wire where
  readPrec = fmap Wire . lift . P.many1 $ do
    wp <- PC.readPrec_to_P readPrec 0
    P.optional . P.char $ ','
    return wp

wirePartToPoints :: (Int, Int) -> Int -> WirePart -> [((Int, Int), Int)]
wirePartToPoints (x, y) steps (WirePart R n) = [((x + dx, y), steps + dx) | dx <- [1 .. n]]
wirePartToPoints (x, y) steps (WirePart L n) = [((x - dx, y), steps + dx) | dx <- [1 .. n]]
wirePartToPoints (x, y) steps (WirePart D n) = [((x, y + dy), steps + dy) | dy <- [1 .. n]]
wirePartToPoints (x, y) steps (WirePart U n) = [((x, y - dy), steps + dy) | dy <- [1 .. n]]

wireToPoints :: Wire -> Map (Int, Int) Int
wireToPoints = doer empty (0, 0) 0 . unWire
  where
    doer ps _ _ [] = ps
    doer ps p steps (wp : w) = doer (unionWith min ps (fromList ps')) p' steps' w
      where
        ps' = wirePartToPoints p steps wp
        (p', steps') = last ps'

wireIntersections :: [Wire] -> Map (Int, Int) [Int]
wireIntersections = foldl1 (intersectionWith (++)) . map (M.map (: []) . wireToPoints)

manhDist :: (Int, Int) -> (Int, Int) -> Int
manhDist p1 p2 = abs (fst p1 - fst p2) + abs (snd p1 - snd p2)

part1Solution :: [Wire] -> Int
part1Solution = manhDist (0, 0) . minimumBy (compare `on` manhDist (0, 0)) . keys . wireIntersections

part2Solution :: [Wire] -> Int
part2Solution = minimum . map sum . M.elems . wireIntersections
