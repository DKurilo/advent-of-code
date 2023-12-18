module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Text.ParserCombinators.ReadP as P
import Text.Read

hex :: String
hex = "0123456789abcdef"

isHex :: Char -> Bool
isHex = (`elem` hex)

data Point = P {px :: Int, py :: Int} deriving (Show, Eq, Ord)

data Direction = DUp | DRight | DDown | DLeft deriving (Show)

stepsInDirection :: Direction -> Point -> Int -> Point
stepsInDirection DUp p n = p {py = py p - n}
stepsInDirection DRight p n = p {px = px p + n}
stepsInDirection DDown p n = p {py = py p + n}
stepsInDirection DLeft p n = p {px = px p - n}

instance Read Direction where
  readPrec =
    lift $
      ( do
          _ <- P.char 'U'
          return DUp
      )
        P.<++ ( do
                  _ <- P.char 'R'
                  return DRight
              )
        P.<++ ( do
                  _ <- P.char 'D'
                  return DDown
              )
        P.<++ ( do
                  _ <- P.char 'L'
                  return DLeft
              )

data Instr = Instr {idir :: Direction, isteps :: Int, ireal :: (Direction, Int)} deriving (Show)

instance Read Instr where
  readPrec = do
    d <- readPrec
    lift P.skipSpaces
    steps <- readPrec
    lift P.skipSpaces
    _ <- lift . P.string $ "(#"
    cs <- lift . P.munch $ isHex
    let realD = case last cs of
          '0' -> DRight
          '1' -> DDown
          '2' -> DLeft
          _ -> DUp
        realSteps = read . ("0x" <>) . take 5 $ cs
    _ <- lift . P.char $ ')'
    return . Instr d steps $ (realD, realSteps)

mkPerimeter :: [Instr] -> [Point]
mkPerimeter = doer (P 0 0)
  where
    doer :: Point -> [Instr] -> [Point]
    doer _ [] = []
    doer p (Instr d n _ : is) =
      stepsInDirection d p n : doer (stepsInDirection d p n) is

findArea :: [Point] -> Int
findArea ps =
  (`div` 2)
    . sum
    . zipWith (\p1 p2 -> (py p1 + py p2) * (px p1 - px p2)) ps
    . tail
    . cycle
    $ ps

preparePerimeter :: [Point] -> [Point]
preparePerimeter ps = zipWith3 adjustPoint ps'' (tail cps'') (drop 2 cps'')
  where
    l = length ps
    firstX = minimum . fmap px $ ps
    firstY = minimum . fmap py . filter ((== firstX) . px) $ ps
    ps' = take l . dropWhile (/= P firstX firstY) . cycle $ ps
    ps''
      | (py . head) ps' == (py . head . tail) ps' = ps'
      | otherwise = head ps' : (tail . reverse) ps'
    cps'' = cycle ps''

adjustPoint :: Point -> Point -> Point -> Point
adjustPoint p1 p2 p3
  | py p1 > py p2 && px p3 > px p2 = p2
  | py p3 > py p2 && px p1 > px p2 = p2 {px = px p2 + 1, py = py p2 + 1}
  | py p3 > py p2 && px p1 < px p2 = p2 {px = px p2 + 1}
  | py p1 > py p2 && px p3 < px p2 = p2 {py = py p2 + 1}
  | py p3 < py p2 && px p1 > px p2 = p2 {py = py p2 + 1}
  | py p1 < py p2 && px p3 > px p2 = p2 {px = px p2 + 1}
  | py p1 < py p2 && px p3 < px p2 = p2 {px = px p2 + 1, py = py p2 + 1}
  | py p3 < py p2 && px p1 < px p2 = p2
  | otherwise = error "oops!"

-- What happens here
-- I'm using Shoelace formula to get area
-- https://en.m.wikipedia.org/wiki/Shoelace_formula
-- There is one problem. If we have plygon like this:
-- .....
-- .   .
-- . ...
-- . .
-- . ...
-- .   .
-- .....
-- vertices defined in coordinates where each vertex is in the middle of sqare
-- but we want to have whole square in result
-- to have it we need to converti it to
-- ------

-- |    |
-- |    |
-- |  ---
-- |  |
-- |  ---
-- |    |
-- |    |
-- ------
-- so whole area will be in polygon
-- and one more thing, to make it working properly we need to ensure we are
-- going through points clockwise
-- that is exactly what preparePerimeter is doing.
-- Finds the most right vertex on the top, makes perimeter starting from this point,
-- checks if perimeter have to be reversed, reverse it in case we need it and
-- then adjust each vertex in the way all points (and no new points) are inside polygon
part1Solution :: [String] -> Int
part1Solution = findArea . preparePerimeter . mkPerimeter . fmap read

fixInstructions :: [Instr] -> [Instr]
fixInstructions = fmap (\i -> i {idir = (fst . ireal) i, isteps = (snd . ireal) i})

part2Solution :: [String] -> Int
part2Solution = findArea . preparePerimeter . mkPerimeter . fixInstructions . fmap read
