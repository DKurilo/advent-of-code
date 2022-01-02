module Lib
  ( part1Solution,
    part2Solution,
    Target (..),
  )
where

import Control.Monad (guard)
import Data.List (nub)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (trace)
import GHC.Float.RealFracMethods (floorDoubleInt)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Target = Target Int Int Int Int deriving (Show)

instance Read Target where
  readPrec = do
    (lift . P.string) "target area: x="
    xStart <- readPrec
    (lift . P.string) ".."
    xEnd <- readPrec
    (lift . P.string) ", y="
    yStart <- readPrec
    (lift . P.string) ".."
    Target xStart xEnd yStart <$> readPrec

findnt :: Target -> Int -> Int -> Maybe Int
findnt (Target xStart xEnd yStart yEnd) xT vx0
  | d < 0 = Nothing
  | nt <= 0 = Nothing
  | xFinal < xStart || xFinal > xEnd = Nothing
  | otherwise = Just nt
  where
    b = fromIntegral $ 2 * vx0 + 1
    c = fromIntegral $ 2 * xT
    d = b * b - 4 * c
    nt = floorDoubleInt $ (b - sqrt d) / 2
    xFinal = if nt <= vx0 then vx0 * nt - (nt * nt - nt) `div` 2 else vx0 * vx0 - (vx0 * vx0 - vx0) `div` 2

findVy0 :: Target -> Int -> Int -> Maybe Int
findVy0 (Target xStart xEnd yStart yEnd) yT nt
  | nt <= 0 = Nothing
  | yFinal < yStart || yFinal > yEnd = Nothing
  | otherwise = Just vy0
  where
    vy0 = floorDoubleInt $ fromIntegral (nt - 1) / 2 + fromIntegral yT / fromIntegral nt
    yFinal = vy0 * nt - (nt * nt - nt) `div` 2

findYMax :: Target -> Int -> Int -> Int -> Maybe Int
findYMax t xMax yMin vx0 = (\vy0 -> (vy0 * vy0 + vy0) `div` 2) <$> (findVy0 t yMin =<< findnt t xMax vx0)

part1Solution :: Target -> Int
part1Solution t@(Target xStart xEnd yStart yEnd) =
  maximum
    . map (fromMaybe 0)
    . concatMap (\vx0 -> map (\xe -> findYMax t xe yStart vx0) [xStart .. xEnd])
    $ [1 .. xEnd]

part2Solution :: Target -> Int
part2Solution t@(Target xStart xEnd yStart yEnd) =
  length
    . nub
    $ do
      vx0 <- [1 .. xEnd]
      n <- [1 .. 10000] -- just huge number
      y <- [yStart .. yEnd]
      let mbvy0 = findVy0 t y n
      guard $ isJust mbvy0
      let vy0 = fromMaybe 1000 mbvy0
      let xFinal = if n <= vx0 then vx0 * n - (n * n - n) `div` 2 else vx0 * vx0 - (vx0 * vx0 - vx0) `div` 2
          yFinal = vy0 * n - (n * n - n) `div` 2
      guard $ xFinal >= xStart && xFinal <= xEnd
      guard $ yFinal >= yStart && yFinal <= yEnd
      return (vx0, vy0)
