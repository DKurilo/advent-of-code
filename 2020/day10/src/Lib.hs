module Lib
    ( part1solution
    , part2solution
    ) where

import           Data.List   (sort)
import qualified Data.Vector as V

checkDiff :: Int -> (Int, Int) -> Bool
checkDiff d (x, y) = (y - x) == d

pathes :: V.Vector Int -> [Int]
pathes xs = [pathes' i | i <- [0..n]]
    where n = V.length xs - 1
          pathes' 0 = 1
          pathes' i = (sum . map p) [0..(i - 1)]
              where p i' = if (xs V.! i - xs V.! i') <= 3 then pathes xs !! i' else 0

input :: IO [Int]
input = (\xs -> xs <> [3 + last xs]) . (0:) . sort . map read . filter (not . null) . lines <$> readFile "input"

part1solution :: IO ()
part1solution = do
    xs <- input
    let xxs = zip xs (tail xs)
        n1 = length . filter (checkDiff 1) $ xxs
        n3 = length . filter (checkDiff 3) $ xxs
    print (n1, n3, n1 * n3)

part2solution :: IO ()
part2solution = do
    xs <- V.fromList <$> input
    print (pathes xs !! (V.length xs - 1))
