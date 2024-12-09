{-# LANGUAGE LambdaCase #-}

module Lib
  ( part1Solution
  , part2Solution
  ) where

data Block
  = File Int Int
  | Emp Int
  deriving (Eq, Show)

parseInput :: String -> [Block]
parseInput cs = doer cs False 0
  where
    doer :: String -> Bool -> Int -> [Block]
    doer [] _ _ = []
    doer (c:cs') fOrE i =
      if fOrE
        then Emp (read [c]) : doer cs' False i
        else File i (read [c]) : doer cs' True (i + 1)

optimize1 :: [Block] -> [Block]
optimize1 bs = doer bs bsR
  where
    bsR = reverse bs
    doer :: [Block] -> [Block] -> [Block]
    doer [] _ = []
    doer _ [] = []
    doer bs' (Emp _:bsR') = doer bs' bsR'
    doer (b@(File i _):bs') bsR'@(bR@(File k _):_)
      | i == k = [bR]
      | i > k = []
      | otherwise = b : doer bs' bsR'
    doer (Emp x:bs') ((File i y):bsR')
      | x > y = File i y : doer (Emp (x - y) : bs') bsR'
      | x < y = File i x : doer bs' (File i (y - x) : bsR')
      | otherwise = File i y : doer bs' bsR'

checksum :: [Block] -> Int
checksum bs = doer bs 0
  where
    doer :: [Block] -> Int -> Int
    doer [] _ = 0
    doer (Emp x:bs') pos = doer bs' (pos + x)
    doer (File i x:bs') pos =
      i * (2 * pos + x - 1) * x `div` 2 + doer bs' (pos + x)

part1Solution :: String -> Int
part1Solution = checksum . optimize1 . parseInput

canInsert :: Block -> [Block] -> Bool
canInsert (Emp _) _ = True
canInsert (File _ x) bs =
  any
    (\case
       Emp y
         | y >= x -> True
       _ -> False)
    bs

insertBlock :: Block -> [Block] -> [Block]
insertBlock (Emp _) bs = bs
insertBlock b@(File _ x) bs = reverse . doer . reverse $ bs
  where
    doer :: [Block] -> [Block]
    doer [] = []
    doer (b'@(Emp y):bs')
      | y > x = b : Emp (y - x) : bs'
      | y < x = b' : doer bs'
      | otherwise = b : bs'
    doer (b'@(File _ _):bs') = b' : doer bs'

optimize2 :: [Block] -> [Block]
optimize2 bs = reverse . doer bsR $ bsR
  where
    bsR = reverse bs
    doer :: [Block] -> [Block] -> [Block]
    doer bs' [] = bs'
    doer [] _ = []
    doer (Emp _:bs') tbs = doer bs' tbs
    doer (b@(File _ x):bs') tbs
      | canInsert b tbs = takeWhile (/= b) tbs <> [Emp x] <> doer bs' tbs''
      | otherwise = takeWhile (/= b) tbs <> [b] <> doer bs' tbs'
      where
        tbs' = tail . dropWhile (/= b) $ tbs
        tbs'' = insertBlock b tbs'

part2Solution :: String -> Int
part2Solution = checksum . optimize2 . parseInput
