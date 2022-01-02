module Lib
  ( part1Solution,
    part2Solution,
  )
where

boolsToInt :: [Bool] -> Int
boolsToInt = doer 0
  where
    doer x [] = x
    doer x (b : bs) = doer ((if b then 1 else 0) + x * 2) bs

commons :: (Int -> Int -> Bool) -> [String] -> [Bool]
commons op xs =
  map (\n -> n `op` (l - n))
    . foldl (\n1s -> zipWith (\n c -> if c == '1' then n + 1 else n) (n1s ++ repeat 0)) []
    $ xs
  where
    l = length xs

part1Solution :: [String] -> Int
part1Solution = findMul . commons (>=)
  where
    findMul bs = boolsToInt bs * (boolsToInt . map not) bs

-- This function is not total
filterRec :: (Int -> Int -> Bool) -> Int -> [String] -> String
filterRec _ _ [cs] = cs
filterRec op n css = filterRec op (n + 1) . filter (\cs -> (bits !! n) == (cs !! n == '1')) $ css
  where
    bits = commons op css

part2Solution :: [String] -> Int
part2Solution xs = ogRating * co2Rating
  where
    ogRating = boolsToInt . map (== '1') . filterRec (>=) 0 $ xs
    co2Rating = boolsToInt . map (== '1') . filterRec (<) 0 $ xs
