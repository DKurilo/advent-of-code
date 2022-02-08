module Lib
  ( part1Solution,
    part2Solution,
    charToAcre,
  )
where

import Data.List (elemIndex, intercalate)
import Data.Vector (Vector (..), fromList, (!))
import qualified Data.Vector as V
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Acre = Open | Lumber | Tree deriving (Eq, Show)

charToAcre :: Char -> Acre
charToAcre '#' = Lumber
charToAcre '|' = Tree
charToAcre _ = Open

acreToChar :: Acre -> Char
acreToChar Lumber = '#'
acreToChar Tree = '|'
acreToChar Open = '.'

vectorize :: [[a]] -> Vector (Vector a)
vectorize = fromList . map fromList

next :: [[Acre]] -> [[Acre]]
next as = [[transform x y | x <- [0 .. V.length (vas ! y) - 1]] | y <- [0 .. V.length vas - 1]]
  where
    vas = vectorize as
    around x y =
      [ vas ! y' ! x'
        | dx <- [-1 .. 1],
          dy <- [-1 .. 1],
          dx /= 0 || dy /= 0,
          let y' = y + dy,
          y' >= 0 && y' < V.length vas,
          let x' = x + dx,
          x' >= 0 && x' < V.length (vas ! y')
      ]
    transform x y
      | a == Open && treesAround >= 3 = Tree
      | a == Open = Open
      | a == Tree && lumberAround >= 3 = Lumber
      | a == Tree = Tree
      | a == Lumber && lumberAround >= 1 && treesAround >= 1 = Lumber
      | a == Lumber = Open
      where
        a = vas ! y ! x
        asAround = around x y
        treesAround = length . filter (== Tree) $ asAround
        lumberAround = length . filter (== Lumber) $ asAround

resourceValue :: [[Acre]] -> Int
resourceValue as = trees * lumbers
  where
    allAs = concat as
    trees = length . filter (== Tree) $ allAs
    lumbers = length . filter (== Lumber) $ allAs

part1Solution :: [[Acre]] -> Int
part1Solution = resourceValue . (!! 10) . iterate next

showAcres :: [[Acre]] -> String
showAcres as = intercalate "\n" [[acreToChar a | a <- las] | las <- as]

part2Solution :: [[Acre]] -> Int
part2Solution as = resourceValue (acres !! (firstInclude + (1000000000 - firstInclude) `mod` (iAcr + 1 - firstInclude)))
  where
    acres = iterate next as
    asBeforeCycle = takeWhile (\(i, a) -> a `notElem` take i acres) . zip [0 ..] $ acres
    (iAcr, beforeRepeatedAcr) = last asBeforeCycle
    repeatedAcr = next beforeRepeatedAcr
    (Just firstInclude) = repeatedAcr `elemIndex` acres
