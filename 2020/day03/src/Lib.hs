module Lib
    ( part1solution
    , part2solution
    ) where


input :: IO [String]
input = filter (not. null) . lines <$> readFile "./input"

countTrees :: Int -> Int -> [String] -> Int
countTrees dx dy slope = sum . map (\y -> if slope !! (y * dy) !! (dx * y `mod` l) == '.' then 0 else 1) $ [0..(length slope - 1) `div` dy]
    where l = (length . head) slope

part1solution :: IO ()
part1solution = print . countTrees 3 1 =<< input

part2solution :: IO ()
part2solution = do
    slope <- input
    print . map (\f -> f slope) $ [countTrees 1 1, countTrees 3 1, countTrees 5 1, countTrees 7 1, countTrees 1 2]
    print . product . map (\f -> f slope) $ [countTrees 1 1, countTrees 3 1, countTrees 5 1, countTrees 7 1, countTrees 1 2]
