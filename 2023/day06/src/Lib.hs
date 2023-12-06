module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Text.ParserCombinators.ReadP as P
import Text.Read

readArr :: P.ReadP [Int]
readArr = P.many $ do
  t <- readPrec_to_P readPrec 0
  P.skipSpaces
  return t

data Race = Race {rTime :: Int, rDistance :: Int} deriving (Show)

newtype Races = Races {unRaces :: [Race]} deriving (Show)

instance Read Races where
  readPrec = lift $ do
    _ <- P.string "Time:"
    P.skipSpaces
    ts <- readArr
    _ <- P.string "Distance:"
    P.skipSpaces
    ds <- readArr
    return . Races $ zipWith Race ts ds

-- getDistance :: Int -> Int -> Int
-- getDistance hold move = move * hold

countWays :: Race -> Int
-- this simple way works perfectly on given data. At least it worked for me
-- countWays r = length . filter (> rDistance r) . fmap (\t -> getDistance t (rTime r - t)) $ [0 .. rTime r]
-- just for fun, here is fast way to do things
countWays r = xMax - xMin + 1
  where
    a = -1 :: Double
    b = fromIntegral . rTime $ r
    c = fromIntegral . negate . (+ 1) . rDistance $ r
    d = sqrt (b * b - 4 * a * c)
    x1 = (-b + d) / (2 * a)
    x2 = (-b - d) / (2 * a)
    xMin = ceiling $ min x1 x2
    xMax = floor $ max x1 x2

part1Solution :: String -> Int
part1Solution = product . fmap countWays . unRaces . read

part2Solution :: String -> Int
part2Solution = product . fmap countWays . unRaces . read . filter (/= ' ')
