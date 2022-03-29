module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (intercalate)
import Data.Set (Set (..), fromList, member, singleton, size, toList, unions)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

biodiversity :: [String] -> Int
biodiversity css =
  sum
    . zipWith
      ( \y ->
          sum
            . zipWith
              ( \x c ->
                  if c == '#'
                    then 2 ^ (x + w * y)
                    else 0
              )
              [0 ..]
      )
      [0 ..]
    $ css
  where
    w = length . head $ css

nextMinute :: [String] -> [String]
nextMinute css = [[nextTile x y | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]
  where
    h = length css
    w = length . head $ css

    adjacentBugs :: Int -> Int -> Int
    adjacentBugs x y =
      sum $
        [1 | let x' = x - 1, x' >= 0, css !! y !! x' == '#']
          ++ [1 | let x' = x + 1, x' < w, css !! y !! x' == '#']
          ++ [1 | let y' = y - 1, y' >= 0, css !! y' !! x == '#']
          ++ [1 | let y' = y + 1, y' < h, css !! y' !! x == '#']

    nextTile :: Int -> Int -> Char
    nextTile x y
      | isBug && bugs == 1 = '#'
      | not isBug && (bugs == 1 || bugs == 2) = '#'
      | otherwise = '.'
      where
        isBug = css !! y !! x == '#'
        bugs = adjacentBugs x y

part1Solution :: [String] -> Int
part1Solution =
  biodiversity
    . head
    . head
    . filter (\(x : xs) -> x `elem` xs)
    . zipWith (\n -> reverse . take n) [2 ..]
    . repeat
    . iterate nextMinute

data Point = Point {pX :: Int, pY :: Int, pDepth :: Int} deriving (Eq, Ord, Show)

newtype World = World {wMap :: Set Point} deriving (Show)

showPlane :: World -> Int -> String
showPlane w d =
  intercalate
    "\n"
    [ [ if x == 2 && y == 2
          then '?'
          else if Point x y d `member` wMap w then '#' else '.'
        | x <- [0 .. 4]
      ]
      | y <- [0 .. 4]
    ]

showWorld :: World -> String
showWorld w = intercalate "\n\n" . map (\d -> "Depth " <> show d <> "\n" <> showPlane w d) $ [minDepth .. maxDepth]
  where
    depthes = S.map pDepth . wMap $ w
    minDepth = S.findMin depthes
    maxDepth = S.findMax depthes

parseWorld :: [String] -> World
parseWorld css = World . fromList $ [Point x y 0 | y <- [0 .. length css - 1], x <- [0 .. length (css !! y) -1], css !! y !! x == '#']

adjacentPoints :: Point -> [Point]
adjacentPoints p = upPs ++ downPs ++ leftPs ++ rightPs
  where
    upPs
      | pY p == 0 = [Point 2 1 (pDepth p - 1)]
      | pY p == 3 && pX p == 2 = map (\x -> Point x 4 (pDepth p + 1)) [0 .. 4]
      | otherwise = [p {pY = pY p - 1}]

    downPs
      | pY p == 4 = [Point 2 3 (pDepth p - 1)]
      | pY p == 1 && pX p == 2 = map (\x -> Point x 0 (pDepth p + 1)) [0 .. 4]
      | otherwise = [p {pY = pY p + 1}]

    leftPs
      | pX p == 0 = [Point 1 2 (pDepth p - 1)]
      | pX p == 3 && pY p == 2 = map (\y -> Point 4 y (pDepth p + 1)) [0 .. 4]
      | otherwise = [p {pX = pX p - 1}]

    rightPs
      | pX p == 4 = [Point 3 2 (pDepth p - 1)]
      | pX p == 1 && pY p == 2 = map (\y -> Point 0 y (pDepth p + 1)) [0 .. 4]
      | otherwise = [p {pX = pX p + 1}]

nextWorld :: World -> World
nextWorld w = World . unions . map bugsInPoint . concatMap adjacentPoints . toList . wMap $ w
  where
    adjacentBugs :: Point -> Int
    adjacentBugs = sum . map (\p -> if p `member` wMap w then 1 else 0) . adjacentPoints

    bugsInPoint :: Point -> Set Point
    bugsInPoint p
      | isBug && bugsAround == 1 = singleton p
      | not isBug && (bugsAround == 1 || bugsAround == 2) = singleton p
      | otherwise = S.empty
      where
        isBug = p `member` wMap w
        bugsAround = adjacentBugs p

part2Solution :: [String] -> Int
part2Solution = size . wMap . (!! 200) . iterate nextWorld . parseWorld
