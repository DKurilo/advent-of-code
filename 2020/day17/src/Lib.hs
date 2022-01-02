module Lib
    ( part1solution
    , part2solution
    ) where

import qualified Data.Set as S

type Coord = [Int]
type EnergySource = S.Set Coord

neighbor :: Int -> [[Int]]
neighbor = filter (not . all (== 0)) . go [[-1], [0], [1]]
    where go :: [[Int]] -> Int -> [[Int]]
          go xs 1 = xs
          go xs n = go (concat  [map (x:) xs | x <- [-1..1]]) (n - 1)

esCycle :: Int -> EnergySource -> EnergySource
esCycle n es = S.foldl go es es
    where go es' xyz = updateSpaceAround xyz (if shouldLive xyz then es' else S.delete xyz es')
          coordsAround xyz = map (zipWith (+) xyz) (neighbor n)
          shouldLive xyz = let m = activeAround xyz in m == 2 || m == 3
          shouldActivate xyz = xyz `S.notMember` es && activeAround xyz == 3
          activeAround = sum . map (\xyz -> if xyz `S.member` es then 1 else 0) . coordsAround
          updateSpaceAround xyz es' = foldl (\es'' xyz -> if shouldActivate xyz then S.insert xyz es'' else es'') es'
                                    . coordsAround $ xyz

nESCycles :: Int -> Int -> EnergySource -> EnergySource
nESCycles n m es = foldl (\es' _ -> esCycle n es') es [1..m]

input :: Int -> IO EnergySource
input n = S.fromList . map fst . filter (\(_,c) -> c == '#')
        . concatMap (\(y,cs) -> zipWith (\x c -> ([x,y] ++ replicate (n - 2) 0, c)) [0..] cs) . zip [0..] . lines
        <$> readFile "input"

part1solution :: IO ()
part1solution = print . S.size . nESCycles 3 6 =<< input 3

part2solution :: IO ()
part2solution = print . S.size . nESCycles 4 6 =<< input 4
