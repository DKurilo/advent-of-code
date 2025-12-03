module Lib (
    part1Solution,
    part2Solution,
)
where

import Data.List.Split (splitOn)
import qualified Data.Set as S

data Range
    = FRange {frLen :: Int}
    | ERange
        { erStart :: Integer
        , erEnd :: Integer
        , erHalfStart :: Integer
        , erHalfEnd :: Integer
        , erMul :: Integer
        }
    | ORange {orStart :: Integer, orEnd :: Integer}
    deriving (Eq, Show)

ranges :: [String] -> [Range]
ranges [xcs, ycs]
    | x > y = []
    | lx < ly =
        ranges [xcs, replicate lx '9']
            <> [FRange l | l <- [lx + 1 .. ly - 1]]
            <> ranges ["1" <> replicate (ly - 1) '0', ycs]
    | lx > ly = []
    | even lx = [ERange x y x2 y2 (10 ^ lx2)]
    | otherwise = [ORange x y]
  where
    lx = length xcs
    lx2 = lx `div` 2
    ly = length ycs
    x = read xcs
    x2 = read . take lx2 $ xcs
    y = read ycs
    y2 = read . take lx2 $ ycs
ranges _ = []

codesFromRange :: Range -> [Integer]
codesFromRange (FRange n)
    | odd n = []
    | otherwise = fmap (\i2 -> i2 * mul + i2) [10 ^ (n `div` 2 - 1) .. 10 ^ (n `div` 2) - 1]
  where
    n2 = n `div` 2
    mul = 10 ^ n2
codesFromRange (ERange x y x2 y2 mul) = [i | i2 <- [x2 .. y2], let i = i2 * mul + i2, i >= x, i <= y]
codesFromRange (ORange _ _) = []

parseRanges :: String -> [Range]
parseRanges = concatMap (ranges . splitOn "-") . splitOn ","

part1Solution :: String -> Integer
part1Solution = sum . concatMap codesFromRange . parseRanges

data Range2 = Range2 {r2Start :: Integer, r2End :: Integer, r2Len :: Int} deriving (Eq, Show)

ranges2 :: [String] -> [Range2]
ranges2 [xcs, ycs]
    | x > y = []
    | lx < ly =
        ranges2 [xcs, replicate lx '9']
            <> concat [ranges2 ["1" <> replicate (l - 1) '0', replicate l '9'] | l <- [lx + 1 .. ly - 1]]
            <> ranges2 ["1" <> replicate (ly - 1) '0', ycs]
    | lx > ly = []
    | otherwise = [Range2 x y lx]
  where
    lx = length xcs
    ly = length ycs
    x = read xcs
    y = read ycs
ranges2 _ = []

nTimes :: Integer -> Integer -> Int -> Integer
nTimes x mul n = doer 0 n
  where
    doer :: Integer -> Int -> Integer
    doer y 0 = y
    doer y n = doer (y * mul + x) (n - 1)

repeatingNthInRange :: Int -> Int -> Integer -> Integer -> [Integer]
repeatingNthInRange n times x y =
    [ k
    | i <- [10 ^ (n - 1) .. 10 ^ n - 1]
    , let k = nTimes i (10 ^ n) times
    , k >= x
    , k <= y
    ]

codesFromRange2 :: Range2 -> [Integer]
codesFromRange2 (Range2 x y n) =
    concat
        [ repeatingNthInRange n' k x y
        | n' <- [1 .. n `div` 2]
        , n `mod` n' == 0
        , let k = n `div` n'
        ]

parseRanges2 :: String -> [Range2]
parseRanges2 = concatMap (ranges2 . splitOn "-") . splitOn ","

part2Solution :: String -> Integer
part2Solution = sum . S.fromList . concatMap codesFromRange2 . parseRanges2
