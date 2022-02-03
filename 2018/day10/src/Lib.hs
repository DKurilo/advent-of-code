module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (intercalate)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {px :: Int, py :: Int, pvx :: Int, pvy :: Int} deriving (Eq, Show)

instance Read Point where
  readPrec = do
    lift . P.string $ "position=<"
    lift P.skipSpaces
    x <- readPrec
    lift . P.char $ ','
    lift P.skipSpaces
    y <- readPrec
    lift . P.string $ "> velocity=<"
    lift P.skipSpaces
    vx <- readPrec
    lift . P.char $ ','
    lift P.skipSpaces
    vy <- readPrec
    lift . P.char $ '>'
    return $ Point x y vx vy

showPoints :: [Point] -> String
showPoints ps = intercalate "\n" [[if any (\p -> px p == x && py p == y) ps then '#' else ' ' | x <- [minX .. maxX]] | y <- [minY .. maxY]]
  where
    minX = minimum . map px $ ps
    maxX = maximum . map px $ ps
    minY = minimum . map py $ ps
    maxY = maximum . map py $ ps

movePoint :: Point -> Point
movePoint p = p {px = px p + pvx p, py = py p + pvy p}

move :: [Point] -> [Point]
move = map movePoint

canBeInteresting :: [Point] -> Bool
canBeInteresting ps = (maxY - minY) < 15
  where
    minY = minimum . map py $ ps
    maxY = maximum . map py $ ps

part1Solution :: [Point] -> String
part1Solution = showPoints . head . filter canBeInteresting . iterate move

part2Solution :: [Point] -> Int
part2Solution = fst . head . filter (canBeInteresting . snd) . zip [0 ..] . iterate move
