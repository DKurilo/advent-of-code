module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Direction = U | D | L | R deriving (Show)

rotateRight :: Direction -> Direction
rotateRight U = R
rotateRight R = D
rotateRight D = L
rotateRight L = U

rotateLeft :: Direction -> Direction
rotateLeft U = L
rotateLeft L = D
rotateLeft D = R
rotateLeft R = U

inverseD :: Direction -> Direction
inverseD U = D
inverseD D = U
inverseD R = L
inverseD L = R

move :: Direction -> (Int, Int) -> (Int, Int)
move U (x, y) = (x, y - 1)
move D (x, y) = (x, y + 1)
move L (x, y) = (x - 1, y)
move R (x, y) = (x + 1, y)

burst :: (((Direction, (Int, Int)), S.Set (Int, Int)), Int) -> (((Direction, (Int, Int)), S.Set (Int, Int)), Int)
burst (((d, pos), infected), cnt)
  | pos `S.member` infected = (((dr, move dr pos), S.delete pos infected), cnt)
  | otherwise = (((dl, move dl pos), S.insert pos infected), cnt + 1)
  where
    dr = rotateRight d
    dl = rotateLeft d

center :: [[a]] -> (Int, Int)
center xss = ((length . head) xss `div` 2, length xss `div` 2)

mkInfected :: [String] -> S.Set (Int, Int)
mkInfected = S.fromList . map fst . filter snd . concat . zipWith (\y -> zipWith (\x c -> ((x, y), c == '#')) [0 ..]) [0 ..]

part1Solution :: [String] -> Int
part1Solution css = snd . (!! 10000) . iterate burst $ (((U, pos), infected), 0)
  where
    pos = center css
    infected = mkInfected css

data Flag = C | W | I | F deriving (Show, Eq)

mkInfected2 :: [String] -> M.Map (Int, Int) Flag
mkInfected2 =
  M.fromList
    . filter ((== I) . snd)
    . concat
    . zipWith (\y -> zipWith (\x c -> ((x, y), if c == '#' then I else C)) [0 ..]) [0 ..]

burst2 :: (((Direction, (Int, Int)), M.Map (Int, Int) Flag), Int) -> (((Direction, (Int, Int)), M.Map (Int, Int) Flag), Int)
burst2 (((d, pos), infected), cnt) = case pos `M.lookup` infected of
  Just W -> (((d, move d pos), M.insert pos I infected), cnt + 1)
  Just I -> (((dr, move dr pos), M.insert pos F infected), cnt)
  Just F -> (((di, move di pos), M.delete pos infected), cnt)
  _ -> (((dl, move dl pos), M.insert pos W infected), cnt)
  where
    dr = rotateRight d
    dl = rotateLeft d
    di = inverseD d

part2Solution :: [String] -> Int
part2Solution css = snd . (!! 10000000) . iterate burst2 $ (((U, pos), infected), 0)
  where
    pos = center css
    infected = mkInfected2 css
