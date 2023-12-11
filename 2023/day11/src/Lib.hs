module Lib
  ( part1Solution,
    part2Solution,
  )
where

transpose :: [(Int, Int)] -> [(Int, Int)]
transpose = fmap (\(x, y) -> (y, x))

expand :: Int -> [(Int, Int)] -> [(Int, Int)]
expand n = doer 0
  where
    doer :: Int -> [(Int, Int)] -> [(Int, Int)]
    doer i gs
      | i < maxI && null gsInRow =
          doer (i + n)
            . fmap
              ( \g@(x, y) ->
                  if x > i
                    then (x + n - 1, y)
                    else g
              )
            $ gs
      | i >= maxI = gs
      | otherwise = doer (i + 1) gs
      where
        maxI = maximum . fmap fst $ gs
        gsInRow = filter ((== i) . fst) gs

galaxies :: [String] -> [(Int, Int)]
galaxies = fmap snd . filter ((== '#') . fst) . concat . zipWith (\y -> zipWith (\x c -> (c, (x, y))) [0 ..]) [0 ..]

pairs :: [a] -> [(a, a)]
pairs xs
  | lastIndex < 1 = []
  | otherwise = [(xs !! i, xs !! j) | i <- [0 .. lastIndex - 1], j <- [i + 1 .. lastIndex]]
  where
    lastIndex = length xs - 1

distance :: ((Int, Int), (Int, Int)) -> Int
distance ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

sumPairsDistances :: Int -> [String] -> Int
sumPairsDistances n = sum . fmap distance . pairs . transpose . expand n . transpose . expand n . galaxies

part1Solution :: [String] -> Int
part1Solution = sumPairsDistances 2

part2Solution :: [String] -> Int
part2Solution = sumPairsDistances 1000000
