module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isDigit)
import Data.List (elemIndex, foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Point = P {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

data PartNumber = PN {pnNumber :: Int, pnCoords :: [Point]} deriving (Eq, Show)

data Engine = Engine {eNumbers :: [PartNumber], eParts :: M.Map Point Char} deriving (Show)

parse :: [String] -> Engine
parse = foldl' parseLine (Engine [] M.empty) . zip [0 ..]

parseLine :: Engine -> (Int, String) -> Engine
parseLine e (y, cs) = fst . foldl' doer (e, False) . zip [0 ..] $ cs
  where
    doer :: (Engine, Bool) -> (Int, Char) -> (Engine, Bool)
    doer (e', wasLastNumber) (x, c)
      | c == '.' = (e', False)
      | isDigit c && wasLastNumber = (e' {eNumbers = (PN {pnNumber = lastNumber', pnCoords = coords'}) : (tail . eNumbers) e'}, True)
      | isDigit c = (e' {eNumbers = (PN {pnNumber = cNumb, pnCoords = [point]}) : eNumbers e'}, True)
      | otherwise = (e' {eParts = (M.insert point c . eParts) e'}, False)
      where
        point = P {pX = x, pY = y}
        cNumb = (fromMaybe 0 . (`elemIndex` ['0' .. '9'])) c
        lastPartNumber = head . eNumbers $ e'
        lastNumber = pnNumber lastPartNumber
        lastNumber' = lastNumber * 10 + cNumb
        coords = pnCoords lastPartNumber
        coords' = point : coords

isPartNumber :: PartNumber -> Engine -> Bool
isPartNumber pn e = foldl' doer False . pnCoords $ pn
  where
    doer :: Bool -> Point -> Bool
    doer True _ = True
    doer False p = isSomethingAround p

    isSomethingAround :: Point -> Bool
    isSomethingAround (P x y) = any isSomething [P (x + dx) (y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]

    isSomething :: Point -> Bool
    isSomething p = M.member p . eParts $ e

part1Solution :: [String] -> Int
part1Solution css = sum . fmap pnNumber . filter (`isPartNumber` engine) . eNumbers $ engine
  where
    engine = parse css

allStars :: Engine -> [Point]
allStars = fmap fst . filter ((== '*') . snd) . M.toList . eParts

getGearParts :: Engine -> Point -> Maybe (Int, Int)
getGearParts e (P x y)
  | length pns == 2 = Just ((pnNumber . head) pns, (pnNumber . last) pns)
  | otherwise = Nothing
  where
    ps = [P (x + dx) (y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0]
    pns = filter (any (`elem` ps) . pnCoords) . eNumbers $ e

part2Solution :: [String] -> Int
part2Solution css = sum . fmap (maybe 0 (uncurry (*)) . getGearParts engine) . allStars $ engine
  where
    engine = parse css
