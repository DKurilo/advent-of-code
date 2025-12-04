module Lib (
    part1Solution,
    part2Solution,
    mkPlan,
)
where

import qualified Data.Set as S

data Plan = Plan
    { pW :: Int
    , pH :: Int
    , pRolls :: S.Set (Int, Int)
    }
    deriving (Eq, Show)

mkPlan :: String -> Plan
mkPlan cs = Plan w h plan
  where
    css = lines cs
    h = length css
    w = length . head $ css
    plan =
        S.fromList
            . concatMap
                ( fmap fst
                    . filter ((== '@') . snd)
                    . (\(j, cs) -> fmap (\(i, c) -> ((i, j), c)) . zip [0 ..] $ cs)
                )
            . zip [0 ..]
            $ css

around :: Int -> Int -> Plan -> Int
around i j p =
    length
        [ ()
        | y <- [-1 .. 1]
        , x <- [-1 .. 1]
        , x /= 0 || y /= 0
        , S.member (i + x, j + y) . pRolls $ p
        ]

removable :: Plan -> S.Set (Int, Int)
removable p =
    S.fromList
        [ (i, j)
        | j <- [0 .. pH p - 1]
        , i <- [0 .. pW p - 1]
        , S.member (i, j) . pRolls $ p
        , around i j p < 4
        ]

part1Solution :: Plan -> Int
part1Solution = S.size . removable

part2Solution :: Plan -> Int
part2Solution p
    | null r = 0
    | otherwise = S.size r + part2Solution (p{pRolls = pRolls p S.\\ r})
  where
    r = removable p
