module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = []
pairs (x1 : x2 : xs) = (x1, x2) : pairs xs

generateData :: [Bool] -> [Bool]
generateData xs = concatMap (\(b1, b2) -> xs ++ (b1 : xs') ++ [b2]) . pairs $ (False : gen [False])
  where
    xs' = (map not . reverse) xs
    gen bs = bs' ++ gen (bs ++ bs')
      where
        bs' = ((False :) . map not . reverse) bs

getChecksum :: [Bool] -> [Bool]
getChecksum xs
  | (odd . length) xs = xs
  | otherwise = getChecksum . map (uncurry (==)) . pairs $ xs

fillAndGetChecksum :: String -> String -> String
fillAndGetChecksum l = map (\b -> if b then '1' else '0') . getChecksum . take (read l) . generateData . map (== '1')

part1Solution :: [String] -> String
part1Solution (xs : l : _) = fillAndGetChecksum l xs

part2Solution :: [String] -> String
part2Solution (xs : _ : l : _) = fillAndGetChecksum l xs
