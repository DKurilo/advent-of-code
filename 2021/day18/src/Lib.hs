module Lib
  ( part1Solution,
    part2Solution,
    SNumber (..),
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data SNumber = N Int | P SNumber SNumber deriving (Show, Eq)

instance Read SNumber where
  readPrec = do
    ( do
        (lift . P.char) '['
        n1 <- readPrec
        (lift . P.char) ','
        n2 <- readPrec
        (lift . P.char) ']'
        return $ P n1 n2
      )
      PC.+++ (N <$> readPrec)

sumSn :: SNumber -> SNumber -> SNumber
sumSn n1 n2 = reduce $ P n1 n2

reduce :: SNumber -> SNumber
reduce n = case explode n of
  Just n' -> reduce n'
  _ -> case split n of
    Just n' -> reduce n'
    _ -> n

explode :: SNumber -> Maybe SNumber
explode = fmap fst . doer 0
  where
    doer :: Int -> SNumber -> Maybe (SNumber, (Int, Int))
    doer _ (N _) = Nothing
    doer 4 (P (N n1) (N n2)) = Just (N 0, (n1, n2))
    doer d (P n1 n2) = case doer (d + 1) n1 of
      Just (n1', (r, l)) -> Just (P n1' (addToLeft l n2), (r, 0))
      _ -> case doer (d + 1) n2 of
        Just (n2', (r, l)) -> Just (P (addToRight r n1) n2', (0, l))
        _ -> Nothing

addToLeft :: Int -> SNumber -> SNumber
addToLeft l (N n) = N (n + l)
addToLeft l (P n1 n2) = P (addToLeft l n1) n2

addToRight :: Int -> SNumber -> SNumber
addToRight r (N n) = N (n + r)
addToRight r (P n1 n2) = P n1 (addToRight r n2)

split :: SNumber -> Maybe SNumber
split (P n1 n2) = case split n1 of
  Just n1' -> Just $ P n1' n2
  _ -> case split n2 of
    Just n2' -> Just $ P n1 n2'
    _ -> Nothing
split (N n)
  | n >= 10 = Just (P (N (n `div` 2)) (N (n `div` 2 + n `mod` 2)))
  | otherwise = Nothing

magnitude :: SNumber -> Int
magnitude (N n) = n
magnitude (P n1 n2) = 3 * magnitude n1 + 2 * magnitude n2

part1Solution :: [SNumber] -> Int
part1Solution [] = -1
part1Solution [n] = magnitude n
part1Solution (n : ns) = magnitude $ foldl sumSn n ns

part2Solution :: [SNumber] -> Int
part2Solution ns = maximum . map magnitude $ [sumSn n1 n2 | n1 <- ns, n2 <- ns, n1 /= n2]
