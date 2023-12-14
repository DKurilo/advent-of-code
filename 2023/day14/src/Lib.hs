module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (intercalate, transpose)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Tile = TRR | TCR | TE deriving (Eq)

instance Read Tile where
  readPrec =
    lift $
      ( do
          _ <- P.char '.'
          return TE
      )
        P.<++ ( do
                  _ <- P.char '#'
                  return TCR
              )
        P.<++ ( do
                  _ <- P.char 'O'
                  return TRR
              )

instance Show Tile where
  show TRR = "O"
  show TCR = "#"
  show TE = "."

showPlatform :: [[Tile]] -> String
showPlatform = intercalate "\n" . fmap (concatMap show)

parse :: [String] -> [[Tile]]
parse = fmap (fmap (read . (: [])))

tiltNorth :: [[Tile]] -> [[Tile]]
tiltNorth ts = (!! lts) . iterate moveStep $ ts
  where
    lts = length ts

    moveStep :: [[Tile]] -> [[Tile]]
    moveStep ts'
      | lts' < 2 = ts'
      | otherwise = r0' : moveStep (r1' : rest)
      where
        lts' = length ts'
        r0 = head ts'
        r1 = head . tail $ ts'
        rest = tail . tail $ ts'
        (r0', r1') =
          unzip
            . zipWith
              ( \t0 t1 ->
                  if t0 == TE && t1 == TRR
                    then (t1, TE)
                    else (t0, t1)
              )
              r0
            $ r1

weight :: [[Tile]] -> Int
weight = sum . zipWith (\w -> (w *) . length . filter (== TRR)) [1 ..] . reverse

part1Solution :: [String] -> Int
part1Solution = weight . tiltNorth . parse

cyclePlatform :: [[Tile]] -> [[Tile]]
cyclePlatform = (!! 4) . iterate (transpose . reverse . tiltNorth)

part2Solution :: [String] -> Int
part2Solution css = weight . (!! endLoop) . iterate cyclePlatform $ platformAtLoopStart
  where
    cycles = iterate cyclePlatform . parse $ css
    firstLoopEndsAt = length . takeWhile (\(i, p) -> p `notElem` take i cycles) . zip [0 ..] $ cycles
    platformAtLoopStart = cycles !! (firstLoopEndsAt + 1)
    firstLoopStartsAt = length . takeWhile (/= platformAtLoopStart) $ cycles
    endLoop = (1000000000 - firstLoopStartsAt) `mod` (firstLoopEndsAt - firstLoopStartsAt + 1)
