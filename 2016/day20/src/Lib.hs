module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

maxAddr = 4294967295

data Range = Range {rangeFrom :: Int, rangeTo :: Int} deriving (Eq, Show)

instance Read Range where
  readPrec = do
    n <- readPrec
    lift . P.char $ '-'
    Range n <$> readPrec

newtype Firewall = Firewall {fwRanges :: [Range]} deriving (Eq, Show)

emptyFirewall = Firewall []

addRange :: Range -> Firewall -> Firewall
addRange r = Firewall . doer r . fwRanges
  where
    doer :: Range -> [Range] -> [Range]
    doer r' [] = [r']
    doer r' (r : rs)
      | rangeTo r' < rangeFrom r - 1 = r' : r : rs
      | rangeFrom r' > rangeTo r + 1 = r : doer r' rs
      | otherwise = doer (Range (min (rangeFrom r) (rangeFrom r')) (max (rangeTo r) (rangeTo r'))) rs

mkFirewall :: [Range] -> Firewall
mkFirewall = foldl (flip addRange) emptyFirewall

findMinAllowed :: Firewall -> Int
findMinAllowed fw = if (rangeFrom . head . fwRanges) fw > 0 then 0 else (rangeTo . head . fwRanges) fw + 1

part1Solution :: [Range] -> Int
part1Solution = findMinAllowed . mkFirewall

part2Solution :: [Range] -> Int
part2Solution =
  (\(allowed, last) -> if last <= maxAddr then allowed + maxAddr - last + 1 else allowed)
    . foldl
      ( \(allowed, current) r ->
          if current < rangeFrom r
            then (allowed + rangeFrom r - current, rangeTo r + 1)
            else (allowed, rangeTo r + 1)
      )
      (0, 0)
    . fwRanges
    . mkFirewall
