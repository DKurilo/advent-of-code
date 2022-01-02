module Lib
  ( part1Solution,
    part2Solution,
  )
where

fuel :: (Int -> Int) -> Int -> [Int] -> Int
fuel dist x = sum . map (\y -> dist . abs $ (x - y))

leastFuel :: (Int -> Int) -> [Int] -> Int
leastFuel dist xs = minimum . map (\x -> fuel dist x xs) $ [minX .. maxX]
  where
    minX = minimum xs
    maxX = maximum xs

part1Solution :: [Int] -> Int
part1Solution = leastFuel id

part2Solution :: [Int] -> Int
part2Solution = leastFuel (\x -> (x + 1) * x `div` 2)
