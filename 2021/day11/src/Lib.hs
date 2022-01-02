module Lib
  ( part1Solution,
    part2Solution,
  )
where

size = 10

chainFlash :: [[Int]] -> ([[Int]], Int)
chainFlash os =
  doer (findFront os) os
  where
    findFront = map snd . filter ((> 9) . fst) . concat . zipWith (\j -> zipWith (\i l -> (l, (i, j))) [0 ..]) [0 ..]
    doer :: [(Int, Int)] -> [[Int]] -> ([[Int]], Int)
    doer front os
      | null front' = (os'', flashes)
      | otherwise = doer front' os''
      where
        os' = [[induct i j | i <- [0 .. (size - 1)]] | j <- [0 .. (size - 1)]]
        flashes = length . filter (== 0) . concat $ os'
        front' = findFront os'
        os'' = map (map (\l -> if l > 9 then 0 else l)) os'
        induct :: Int -> Int -> Int
        induct i j
          | l == 0 = l
          | l > 9 = l
          | otherwise =
            l
              + sum
                [ if (i', j') `elem` front && os !! j' !! i' == 0 then 1 else 0
                  | i' <- [(i - 1) .. (i + 1)],
                    j' <- [(j - 1) .. (j + 1)],
                    (i', j') /= (i, j) && i' >= 0 && i' < size && j' >= 0 && j' < size
                ]
          where
            l = os !! j !! i

simulate1Day :: [[Int]] -> ([[Int]], Int)
simulate1Day os = chainFlash os'
  where
    os' = map (map (+ 1)) os

simulate :: [[Int]] -> [([[Int]], Int)]
simulate os = (os', flashes) : simulate os'
  where
    (os', flashes) = simulate1Day os

part1Solution :: [[Int]] -> Int
part1Solution = sum . map snd . take 100 . simulate

part2Solution :: [[Int]] -> Int
part2Solution = (+ 1) . length . takeWhile (any (> 0) . concat . fst) . simulate
