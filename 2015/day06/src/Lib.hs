{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (guard)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

type X = Int

type Y = Int

type Point = (X, Y)

data Instr = On Point Point | Off Point Point | Toggle Point Point deriving (Show)

instance Read Instr where
  readPrec = do
    itype <-
      ((lift . P.string) "turn on" >> return On)
        PC.+++ ((lift . P.string) "turn off" >> return Off)
        PC.+++ ((lift . P.string) "toggle" >> return Toggle)
    (lift . P.char) ' '
    let readPoint = do
          x <- readPrec
          (lift . P.char) ','
          y <- readPrec
          return (x, y)
    p <- readPoint
    (lift . P.string) " through "
    itype p <$> readPoint

subLine :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
subLine (a1, a2) (b1, b2)
  | b1 <= a1 && b2 < a2 = [(b2 + 1, a2)]
  | b1 > a1 && b2 >= a2 = [(a1, b1 - 1)]
  | b1 > a1 && b2 < a2 = [(a1, b1 - 1), (b2 + 1, a2)]
  | b1 <= a1 && b2 >= a2 = []
  | otherwise = [(a1, a2)]

overlaps :: (Point, Point) -> (Point, Point) -> Bool
overlaps ((xa1, ya1), (xa2, ya2)) ((xb1, yb1), (xb2, yb2)) = overlapped (xa1, xa2) (xb1, xb2) && overlapped (ya1, ya2) (yb1, yb2)
  where
    overlapped (a1, a2) (b1, b2)
      | b1 <= a1 && b2 >= a1 = True
      | b1 <= a2 && b2 >= a2 = True
      | b1 >= a1 && b2 <= a2 = True
      | otherwise = False

subBox :: (Point, Point) -> (Point, Point) -> [(Point, Point)]
subBox ba@((xa1, ya1), (xa2, ya2)) bb@((xb1, yb1), (xb2, yb2))
  | overlaps ba bb = [((x1, ya1), (x2, ya2)) | (x1, x2) <- xs] ++ [((x1', y1), (x2', y2)) | (y1, y2) <- ys]
  | otherwise = [ba]
  where
    xs = (xa1, xa2) `subLine` (xb1, xb2)
    (x1', x2') = foldl (\l1 l2 -> head $ l1 `subLine` l2) (xa1, xa2) xs
    ys = (ya1, ya2) `subLine` (yb1, yb2)

applyInstr :: Instr -> [(Point, Point)] -> [(Point, Point)]
applyInstr (Off p1 p2) boxes = concatMap (`subBox` (p1, p2)) boxes
applyInstr (Toggle p1 p2) boxes = boxes' ++ boxes''
  where
    boxes' = applyInstr (Off p1 p2) boxes
    boxes'' = foldl (\bs b -> concatMap (`subBox` b) bs) [(p1, p2)] boxes
applyInstr (On p1 p2) boxes = doer [(p1, p2)] boxes
  where
    doer :: [(Point, Point)] -> [(Point, Point)] -> [(Point, Point)]
    doer bs [] = bs
    doer bs (b : bs') = b : doer (concatMap (`subBox` b) bs) bs'

count = sum . map (\((x1, y1), (x2, y2)) -> (x2 - x1 + 1) * (y2 - y1 + 1))

part1Solution :: [Instr] -> Int
part1Solution = count . foldl (flip applyInstr) []

applyInstr2 :: Instr -> [((Point, Point), Int)] -> [((Point, Point), Int)]
applyInstr2 (Off p1 p2) boxes = concatMap doer boxes
  where
    doer :: ((Point, Point), Int) -> [((Point, Point), Int)]
    doer (b, n) = map (,n) bs ++ if n > 1 then map (,n - 1) bs' else []
      where
        bs = b `subBox` (p1, p2)
        bs' = foldl (\bs'' b' -> concatMap (`subBox` b') bs'') [b] bs
applyInstr2 (Toggle p1 p2) boxes = doer [(p1, p2)] boxes
  where
    doer :: [(Point, Point)] -> [((Point, Point), Int)] -> [((Point, Point), Int)]
    doer bs [] = map (,2) bs
    doer bs ((b, n) : bs') = map (,n) notOverlappedBs ++ map (,n + 2) overlappedBs ++ doer newBs bs'
      where
        newBs = concatMap (`subBox` b) bs
        overlappedBs = foldl (\boxes b' -> concatMap (`subBox` b') boxes) bs newBs
        notOverlappedBs = foldl (\boxes b' -> concatMap (`subBox` b') boxes) [b] overlappedBs
applyInstr2 (On p1 p2) boxes = doer [(p1, p2)] boxes
  where
    doer :: [(Point, Point)] -> [((Point, Point), Int)] -> [((Point, Point), Int)]
    doer bs [] = map (,1) bs
    doer bs ((b, n) : bs') = map (,n) notOverlappedBs ++ map (,n + 1) overlappedBs ++ doer newBs bs'
      where
        newBs = concatMap (`subBox` b) bs
        overlappedBs = foldl (\boxes b' -> concatMap (`subBox` b') boxes) bs newBs
        notOverlappedBs = foldl (\boxes b' -> concatMap (`subBox` b') boxes) [b] overlappedBs

count2 = sum . map (\(((x1, y1), (x2, y2)), n) -> (x2 - x1 + 1) * (y2 - y1 + 1) * n)

part2Solution :: [Instr] -> Int
part2Solution = count2 . foldl (flip applyInstr2) []
