module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', foldl1', nub)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Moon = Moon {mX :: Int, mY :: Int, mZ :: Int, mvX :: Int, mvY :: Int, mvZ :: Int} deriving (Show)

instance Read Moon where
  readPrec = do
    lift . P.string $ "<x="
    x <- readPrec
    lift . P.string $ ", y="
    y <- readPrec
    lift . P.string $ ", z="
    z <- readPrec
    lift . P.char $ '>'
    return $ Moon x y z 0 0 0

pot :: Moon -> Int
pot m = (abs . mX) m + (abs . mY) m + (abs . mZ) m

kin :: Moon -> Int
kin m = (abs . mvX) m + (abs . mvY) m + (abs . mvZ) m

total :: Moon -> Int
total m = pot m * kin m

applyG :: Int -> Int -> Int -> Int
applyG v1 x1 x2
  | x2 > x1 = v1 + 1
  | x2 < x1 = v1 - 1
  | otherwise = v1

applyGravity :: Moon -> Moon -> Moon
applyGravity impactor moon =
  moon
    { mvX = applyG (mvX moon) (mX moon) (mX impactor),
      mvY = applyG (mvY moon) (mY moon) (mY impactor),
      mvZ = applyG (mvZ moon) (mZ moon) (mZ impactor)
    }

applyVelocity :: Moon -> Moon
applyVelocity moon = moon {mX = mX moon + mvX moon, mY = mY moon + mvY moon, mZ = mZ moon + mvZ moon}

simuS :: [(Int, Int)] -> [(Int, Int)]
simuS nvs = map (\(n, v) -> (\v' -> (n + v', v')) . foldl' (\v' (n', _) -> applyG v' n n') v $ nvs) nvs

simuStep :: [Moon] -> [Moon]
simuStep ms = map (\m -> applyVelocity . foldl' (flip applyGravity) m $ ms) ms

findCycle :: [(Int, Int)] -> Int
findCycle nvs = (+ 1) . length . takeWhile (/= nvs) . drop 1 . iterate simuS $ nvs

commonPeriod :: Int -> Int -> Int
commonPeriod i j = i * j `div` cd
  where
    cd = gcd i j

part1Solution :: [Moon] -> Int
part1Solution = sum . map total . (!! 1000) . iterate simuStep

part2Solution :: [Moon] -> Int
part2Solution moons =
  foldl1' commonPeriod
    . map findCycle
    $ [map (\m -> (mX m, mvX m)) moons, map (\m -> (mY m, mvY m)) moons, map (\m -> (mZ m, mvZ m)) moons]
