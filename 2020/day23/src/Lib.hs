module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Monad               (foldM_)
import qualified Data.Vector.Generic         as GV
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M

input :: IO [Int]
input =  map (read . (:[])) . filter (/='\n') <$> readFile "input"

rotateCW :: [Int] -> [Int]
rotateCW xs = last xs : init xs

rotateCCW :: [Int] -> [Int]
rotateCCW xs = tail xs ++ [head xs]

move :: [Int] -> [Int]
move xs = rotateCCW (takeWhile (/=destinationCup) remainedCups
                    ++ (destinationCup : removedCups)
                    ++ (tail . dropWhile (/=destinationCup)) remainedCups)
    where minCup = minimum xs
          maxCup = maximum xs
          currentCup = head xs
          removedCups = take 3 . tail $ xs
          remainedCups = head xs : drop 4 xs
          destinationCup = findDestination currentCup
          findDestination x | x' `elem` removedCups = findDestination x'
                            | otherwise = x'
              where x' = if x == minCup then maxCup else x - 1

part1solution :: IO ()
part1solution = do
    cups <- input
    print . concatMap show . tail . until ((==1) . head) rotateCW . foldr (const move) cups $ [0..99]

findDestinationCup maxCup removed n
  | n' `elem` removed = findDestinationCup maxCup removed n'
  | otherwise = n'
    where n' = if n == 1 then maxCup else n - 1

move' cups maxCup currentCup = do
    r1 <- M.read cups (currentCup - 1)
    r2 <- M.read cups (r1 - 1)
    r3 <- M.read cups (r2 - 1)
    let dc = findDestinationCup maxCup [r1, r2, r3] currentCup
    dc' <- M.read cups (dc - 1)
    newCurrentCup <- M.read cups (r3 - 1)
    M.write cups (currentCup - 1) newCurrentCup
    M.write cups (dc - 1) r1
    M.write cups (r3 - 1) dc'
    return newCurrentCup

play moves cups initialCup = do
    let len = length cups
        maxCup = len
    vCups <- M.new len
    mapM_ (uncurry (M.write vCups)) cups
    foldM_ (\c _ -> move' vCups maxCup c) initialCup [0..moves]
    GV.freeze vCups

part2solution :: IO ()
part2solution = do
    cups <- input
    let maxCup = maximum cups
        rest = map (\x -> (x, x + 2)) [maxCup..999998] ++ [(999999, head cups)]
        cups' = [((cups !! i) - 1, cups !! (i + 1)) | i <- [0..length cups - 2]] ++ [(last cups - 1, maxCup + 1)] ++ rest
    cups <- play 10000000 cups' (head cups)
    let n1 = cups V.!  0
        n2 = cups V.! (n1 - 1)
    print (n1 * n2, n1, n2)
