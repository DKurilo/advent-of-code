module Lib
  ( part1Solution,
    part2Solution,
    Image (..),
  )
where

import qualified Data.Vector as V
import Debug.Trace (trace)

type Algorithm = V.Vector Char

data Image = Image Char (V.Vector (V.Vector Char)) deriving (Show)

binToInt :: String -> Int
binToInt = foldl (\n c -> n * 2 + if c == '0' then 0 else 1) 0

getAround :: Image -> Int -> Int -> Int
getAround (Image bg xss) x y =
  binToInt
    [ if x' < 0 || x' >= V.length (xss V.! 0) || y' < 0 || y' >= V.length xss
        then bg
        else xss V.! y' V.! x'
      | y' <- [(y - 1) .. (y + 1)],
        x' <- [(x - 1) .. (x + 1)]
    ]

enchance :: Algorithm -> Image -> [Image]
enchance alg i@(Image bg xss) = i : enchance alg i'
  where
    bg' = if bg == '0' then alg V.! 0 else alg V.! 511
    xss' = V.fromList [V.fromList [alg V.! getAround i x y | x <- [(-1) .. (V.length (xss V.! 0))]] | y <- [(-1) .. (V.length xss)]]
    i' = Image bg' xss'

part1Solution :: Algorithm -> Image -> Int
part1Solution alg = (\(Image _ xss) -> V.length . V.filter (== '1') . V.concat . V.toList $ xss) . (!! 2) . enchance alg

part2Solution :: Algorithm -> Image -> Int
part2Solution alg = (\(Image _ xss) -> V.length . V.filter (== '1') . V.concat . V.toList $ xss) . (!! 50) . enchance alg
