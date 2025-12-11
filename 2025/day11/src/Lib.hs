module Lib (
    part1Solution,
    part2Solution,
    mkGraph,
)
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

mkGraph :: String -> M.Map String [String]
mkGraph =
    M.fromList
        . fmap
            ( \cs' ->
                let (from : to) = words cs'
                 in (init from, to)
            )
        . lines

findAllPaths :: String -> String -> M.Map String [String] -> Int
findAllPaths src dst gr = fromMaybe 0 . M.lookup dst . doer (M.singleton src 1) $ M.empty
  where
    doer :: M.Map String Int -> M.Map String Int -> M.Map String Int
    doer front visited
        | M.null front' = visited'
        | otherwise = doer front' visited'
      where
        front' =
            foldl'
                ( \fr' (cs, x) -> case M.lookup cs gr of
                    Just ns -> foldl' (\fr'' n -> M.insertWith (+) n x fr'') fr' ns
                    Nothing -> fr'
                )
                M.empty
                . M.toList
                $ front
        visited' = foldl' (\v' (cs, x) -> M.insertWith (+) cs x v') visited . M.toList $ front

part1Solution :: M.Map String [String] -> Int
part1Solution = findAllPaths "you" "out"

part2Solution :: M.Map String [String] -> Int
part2Solution gr = svrToFft * fftToDac * dacToOut + svrToDac * dacToFft * fftToOut
    where
        svrToFft = findAllPaths "svr" "fft" gr
        fftToDac = findAllPaths "fft" "dac" gr
        dacToOut = findAllPaths "dac" "out" gr
        svrToDac = findAllPaths "svr" "dac" gr
        dacToFft = findAllPaths "dac" "fft" gr
        fftToOut = findAllPaths "fft" "out" gr
