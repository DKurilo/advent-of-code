module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Three = Three Int Int Int

instance Read Three where
  readPrec = do
    lift P.skipSpaces
    n1 <- readPrec
    lift P.skipSpaces
    n2 <- readPrec
    lift P.skipSpaces
    Three n1 n2 <$> readPrec

part1Solution :: [Three] -> Int
part1Solution = length . filter (\(Three n1 n2 n3) -> n1 + n2 > n3 && n2 + n3 > n1 && n1 + n3 > n2)

convert1to2 :: [Three] -> [Three]
convert1to2 [] = []
convert1to2 ((Three n11 n12 n13) : (Three n21 n22 n23) : (Three n31 n32 n33) : ths) =
  Three n11 n21 n31 : Three n12 n22 n32 : Three n13 n23 n33 : convert1to2 ths
convert1to2 _ = []

part2Solution :: [Three] -> Int
part2Solution = part1Solution . convert1to2
