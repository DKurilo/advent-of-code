module Lib
  ( part1Solution,
    part2Solution,
    Present (..),
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

type Length = Int

type Width = Int

type Height = Int

data Present = Present Length Width Height deriving (Show)

instance Read Present where
  readPrec = do
    l <- readPrec
    (lift . P.char) 'x'
    w <- readPrec
    (lift . P.char) 'x'
    Present l w <$> readPrec

wrapArea :: Present -> Int
wrapArea (Present l w h) = 2 * w1 + 2 * w2 + 2 * w3 + minimum [w1, w2, w3]
  where
    w1 = l * w
    w2 = w * h
    w3 = h * l

ribbonHeight :: Present -> Int
ribbonHeight (Present l w h) = minimum [l1, l2, l3] + l * w * h
  where
    l1 = l + l + w + w
    l2 = l + l + h + h
    l3 = w + w + h + h

part1Solution :: [Present] -> Int
part1Solution = sum . map wrapArea

part2Solution :: [Present] -> Int
part2Solution = sum . map ribbonHeight
