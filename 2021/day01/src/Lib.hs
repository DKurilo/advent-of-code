module Lib
  ( part1Solution,
    part2Solution,
  )
where

countDeeps :: Int -> IO String
countDeeps n =
  show . snd
    . foldl
      ( \(deeps, cnt) s ->
          let x = read s :: Int
              l = length deeps
              prevSum = sum deeps
              nextDeeps = (x : (if l == n then init else id) deeps)
              currentSum = sum nextDeeps
           in if l == n && currentSum > prevSum
                then (nextDeeps, cnt + 1)
                else (nextDeeps, cnt)
      )
      ([], 0)
    . lines
    <$> readFile "./input"

part1Solution :: IO String
part1Solution = countDeeps 1

part2Solution :: IO String
part2Solution = countDeeps 3
