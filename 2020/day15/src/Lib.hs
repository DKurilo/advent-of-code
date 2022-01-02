module Lib
    ( part1solution
    , part2solution
    ) where

import           Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

play :: [Int] -> [Int]
play xs = xs' ++  go (M.fromList $ zip xs' [0..]) x (length xs - 1)
    where xs' = init xs
          x = last xs
          go :: M.Map Int Int -> Int -> Int -> [Int]
          go ys y i = case y `M.lookup` ys of
                        Just k -> y:go ys' (i - k) (i + 1)
                        _      -> y:go ys' 0 (i + 1)
              where ys' = M.insert y i ys

input :: IO [Int]
input = map read . splitOn "," . head . lines <$> readFile "input"

part1solution :: IO ()
part1solution = print . (!!(2020 - 1)) .  play =<< input

-- Well.. Most probably there is a faster way to do it.
-- But I start thinking about it and while I was thinking it was calculated.
-- So I decided I don't have a time to make it in a proper way right now. Maybe I'll do it later.
part2solution :: IO ()
part2solution = print . (!!(30000000 - 1)) . play =<< input
