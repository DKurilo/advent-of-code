module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

toFind :: String
toFind = "XMAS"

data Point = P {px :: Int, py :: Int} deriving (Eq, Ord, Show)

toMap :: [String] -> M.Map Point Char
toMap css = M.fromList [(P x y, css !! y !! x) | x <- [0..lx - 1], y <- [0..ly - 1]]
    where
        ly = length css
        lx = length . head $ css

isInside :: Point -> Point -> Point -> Bool
isInside tl br p = px p >= px tl && py p >= py tl && px p <= px br && py p <= py br

getSubstr :: Int -> Point -> Point -> Point -> (Point -> Point) -> [Point]
getSubstr n tl br start next = filter (isInside tl br) . take n . iterate next $ start

get8Substr :: Int -> Point -> Point -> Point -> [[Point]]
get8Substr n tl br start = filter ((==n) . length) [
        getSubstr n tl br start (\p -> p {px=px p + 1, py=py p}),
        getSubstr n tl br start (\p -> p {px=px p - 1, py=py p}),
        getSubstr n tl br start (\p -> p {px=px p, py=py p + 1}),
        getSubstr n tl br start (\p -> p {px=px p, py=py p - 1}),
        getSubstr n tl br start (\p -> p {px=px p + 1, py=py p + 1}),
        getSubstr n tl br start (\p -> p {px=px p + 1, py=py p - 1}),
        getSubstr n tl br start (\p -> p {px=px p - 1, py=py p + 1}),
        getSubstr n tl br start (\p -> p {px=px p - 1, py=py p - 1})
    ]

part1Solution :: [String] -> Int
part1Solution css = length
                  . filter (==toFind)
                  . fmap (fmap (\p -> fromMaybe '#' . M.lookup p $ mcs))
                  . concatMap pss
                  $ [p | x <- [0..lx - 1], y <- [0..ly - 1], let p = P x y, p `M.lookup` mcs == Just 'X']
    where
        ly = length css
        lx = length . head $ css
        mcs = toMap css
        pss = get8Substr (length toFind) (P 0 0) (P (lx - 1) (ly - 1))

getX :: Point -> [Point]
getX p = [
        p {px=px p - 1, py=py p - 1},
        p {px=px p + 1, py=py p + 1},
        p {px=px p + 1, py=py p - 1},
        p {px=px p - 1, py=py p + 1}
    ]

variantsToFind :: [String]
variantsToFind = ["MSMS", "MSSM", "SMMS", "SMSM"]

part2Solution :: [String] -> Int
part2Solution css = length
                  . filter (`elem` variantsToFind)
                  . fmap (fmap (\p -> fromMaybe '#' . M.lookup p $ mcs))
                  . filter (all (isInside tl br))
                  . fmap getX
                  $ [p | x <- [0..lx - 1], y <- [0..ly - 1], let p = P x y, p `M.lookup` mcs == Just 'A']
    where
        ly = length css
        lx = length . head $ css
        tl = P 0 0
        br = P (lx - 1) (ly - 1)
        mcs = toMap css
