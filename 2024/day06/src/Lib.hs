module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.Set as S
import Debug.Trace (trace)

data Point = P {px :: Int, py :: Int} deriving (Eq, Ord, Show)

data Dir = N | E | S | W deriving (Eq, Ord, Show)

data Lab = Lab { ltl :: Point
               , lbr :: Point
               , lobs :: S.Set Point
               , lgrd :: (Point, Dir)
               , lpth :: S.Set (Point, Dir)
               } deriving (Show)

isExited :: Lab -> Bool
isExited lab = px p < px tl || px p > px br || py p < py tl || py p > py br
    where
        p = (fst . lgrd) lab
        tl = ltl lab
        br = lbr lab

isCycled :: Lab -> Bool
isCycled lab = lgrd lab `S.member` lpth lab

parseMap :: [String] -> Lab
parseMap css = Lab tl br
                   (S.fromList . fmap fst . filter ((=='#') . snd) $ labPs)
                   (fst . head . filter ((=='^') . snd) $ labPs, N)
                   S.empty
    where
        tl = P 0 0
        h = length css
        w = length . head $ css
        br = P (w - 1) (h - 1)
        labPs = [ (p, t)
                | x <- [px tl .. px br]
                , y <- [py tl .. py br]
                , let t = css !! y !! x
                , let p = P x y
                , t == '#' || t == '^'
                ]

move :: Point -> Dir -> Point
move p N = p {py = py p - 1}
move p E = p {px = px p + 1}
move p S = p {py = py p + 1}
move p W = p {px = px p - 1}

rot :: Dir -> Dir
rot N = E
rot E = S
rot S = W
rot W = N

next :: Lab -> Lab
next lab
    | grd' `S.member` lobs lab = lab { lgrd = (grd, dir') }
    | otherwise = lab { lgrd = (grd', dir), lpth = S.insert (lgrd lab) (lpth lab) }
    where
        (grd, dir) = lgrd lab
        grd' = move grd dir
        dir' = rot dir

part1Solution :: [String] -> Int
part1Solution = S.size . S.map fst . lpth . head . dropWhile (not . isExited) . iterate next . parseMap

part2Solution :: [String] -> Int
part2Solution css = length [ p
                           | p <- path
                           , p /= (fst . lgrd) lab
                           , p `S.notMember` lobs lab
                           , let lab' = lab { lobs = p `S.insert` lobs lab }
                           , let lab'' = head . dropWhile (\l -> not (isExited l || isCycled l)) . iterate next $ lab'
                           , isCycled lab''
                           ]
    where
        lab = parseMap css
        path = S.toList . S.map fst . lpth . head . dropWhile (not . isExited) . iterate next $ lab
