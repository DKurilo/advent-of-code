module Lib
    ( solution
    ) where

import           Control.Monad   (forM_)
import           Data.SortedList as SL (SortedList (..), delete, elemOrd,
                                        filter, fromSortedList, map,
                                        toSortedList)

findAddition :: Int -> Int -> SortedList Int -> [[Int]]
findAddition 2 y xs = (fromSortedList . SL.map (\x -> [x, y - x]) . SL.filter check) xs
    where check x = elemOrd (y - x) (delete x xs)
findAddition n y xs = foldl (\ps x -> let found = find x in if null found
                                                              then ps
                                                              else Prelude.map (x:) found ++ ps) [] xs
    where find x = findAddition (n - 1) (y - x) (delete x xs)

solution :: Int -> IO ()
solution n = do
    xs <- toSortedList . Prelude.map read . words <$> readFile "./input"
    forM_ ((Prelude.map  (\ys -> (ys, product ys)) . findAddition n 2020) xs) print
