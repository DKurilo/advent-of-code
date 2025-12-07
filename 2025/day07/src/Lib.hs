{-# LANGUAGE TupleSections #-}

module Lib (
    part1Solution,
    part2Solution,
)
where

import qualified Data.Map as M
import qualified Data.Set as S

processLevel :: String -> S.Set Int -> (Int, S.Set Int)
processLevel cs =
    foldl'
        ( \(splits, beams') x ->
            if cs !! x == '^'
                then (splits + 1, splitBeam x beams')
                else (splits, S.insert x beams')
        )
        (0, S.empty)
        . S.toList
  where
    l = length cs

    splitBeam :: Int -> S.Set Int -> S.Set Int
    splitBeam x = (if x > 0 then S.insert (x - 1) else id) . (if x < l - 1 then S.insert (x + 1) else id)

startingBeams :: String -> S.Set Int
startingBeams = S.fromList . fmap fst . filter ((== 'S') . snd) . zip [0 ..]

part1Solution :: [String] -> Int
part1Solution levels =
    fst
        . foldl'
            ( \(splits, beams) level ->
                let (splits', beams') = processLevel level beams
                 in (splits + splits', beams')
            )
            (0, startingBeams . head $ levels)
        . tail
        $ levels

findPaths :: M.Map Int Int -> String -> M.Map Int Int
findPaths beams cs =
    foldl'
        ( \beams' (x, n) ->
            if cs !! x == '^'
                then splitBeam x n beams'
                else M.insertWith (+) x n beams'
        )
        M.empty
        . M.toList
        $ beams
  where
    l = length cs

    splitBeam :: Int -> Int -> M.Map Int Int -> M.Map Int Int
    splitBeam x n =
        (if x > 0 then M.insertWith (+) (x - 1) n else id)
            . (if x < l - 1 then M.insertWith (+) (x + 1) n else id)

part2Solution :: [String] -> Int
part2Solution levels =
    sum
        . foldl'
            findPaths
            (M.fromList . fmap (,1) . S.toList . startingBeams . head $ levels)
        . tail
        $ levels
