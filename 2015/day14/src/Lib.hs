{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

type Name = String

type Speed = Int

type Interval = Int

data Reindeer = Reindeer Name Speed Interval Interval deriving (Show)

instance Read Reindeer where
  readPrec = do
    cs <- (lift . P.munch1) isAlpha
    (lift . P.string) " can fly "
    s <- readPrec
    (lift . P.string) " km/s for "
    si <- readPrec
    (lift . P.string) " seconds, but then must rest for "
    ri <- readPrec
    (lift . P.string) " seconds."
    return $ Reindeer cs s si ri

calculateDist :: Interval -> Reindeer -> Int
calculateDist t (Reindeer _ sp spI rI) = cycles * distPerCycle + distRest
  where
    cycleTime = spI + rI
    cycles = t `div` cycleTime
    distPerCycle = sp * spI
    rest = t `mod` cycleTime
    distRest = if rest >= spI then distPerCycle else rest * sp

part1Solution :: [Reindeer] -> Int
part1Solution = maximum . map (calculateDist 2503)

race :: Interval -> [Reindeer] -> [(Reindeer, Int)]
race 0 rs = map (,0) rs
race n rs = map (\(r, p) -> (r, p + if calculateDist n r == best then 1 else 0)) prev
  where
    prev = race (n - 1) rs
    best = maximum . map (calculateDist n) $ rs

part2Solution :: [Reindeer] -> Int
part2Solution = maximum . map snd . race 2503
