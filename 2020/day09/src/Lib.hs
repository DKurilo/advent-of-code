module Lib
    ( part1solution
    , part2solution
    ) where

import           Data.Vector as V (Vector (..), fromList, head, ifilter, imap,
                                   length, maximum, minimum, slice, sum, toList,
                                   (!))

findWrongs :: Int -> Vector Int -> Vector Int
findWrongs preambleLength xs = ifilter go xs
    where go i x
            | i < preambleLength = False
            | otherwise = x `notElem` [xs V.! i1 + xs V.! i2 | i1 <- preamble, i2 <- preamble, i1 /= i2]
              where preamble = [(i - preambleLength)..(i - 1)]

findAddends :: Int -> Vector Int -> [Vector Int]
findAddends y xs = filter (\xs' -> V.sum xs' == y) . concat . V.toList
                 . imap (\i _ -> [slice i k xs | k <- [2..(V.length xs - i)]]) $ xs

input :: IO (Vector Int)
input = fromList . map read .filter (not . null) . lines <$> readFile "input"

part1solution :: IO ()
part1solution = print . findWrongs 25 =<< input

part2solution :: IO ()
part2solution = do
    xs <- input
    let y = V.head . findWrongs 25 $ xs
        addends = findAddends y xs
        weaknesses = map (\xs' -> V.minimum xs' + V.maximum xs') addends
    print weaknesses
