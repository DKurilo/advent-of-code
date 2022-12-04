module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Range = Range {rFrom :: Int, rTo :: Int}

instance Read Range where
  readPrec = do
    from <- readPrec
    _ <- lift . P.char $ '-'
    Range from <$> readPrec

data Pair = Pair Range Range

instance Read Pair where
  readPrec = do
    range <- readPrec
    _ <- lift . P.char $ ','
    Pair range <$> readPrec

isInside :: Range -> Range -> Bool
isInside r1 r2 = rFrom r1 <= rFrom r2 && rTo r1 >= rTo r2

areOverlapped :: Range -> Range -> Bool
areOverlapped r1 r2 = (isDotInside . rFrom) r1 || (isDotInside . rTo) r1
  where
    isDotInside n = n >= rFrom r2 && n <= rTo r2

contains :: Pair -> Bool
contains (Pair r1 r2) = isInside r1 r2 || isInside r2 r1

overlapped :: Pair -> Bool
overlapped (Pair r1 r2) = areOverlapped r1 r2 || areOverlapped r2 r1

part1Solution :: [Pair] -> Int
part1Solution = length . filter contains

part2Solution :: [Pair] -> Int
part2Solution = length . filter overlapped
