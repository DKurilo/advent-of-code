module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', scanl')
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read hiding (step)

data Step = N | NE | SE | S | SW | NW deriving (Show)

instance Read Step where
  readPrec =
    lift $
      (P.string "ne" >> return NE)
        P.+++ (P.string "nw" >> return NW)
        P.+++ (P.string "se" >> return SE)
        P.+++ (P.string "sw" >> return SW)
        P.<++ (P.char 's' >> return S)
        P.<++ (P.char 'n' >> return N)

newtype Path = Path {pSteps :: [Step]} deriving (Show)

instance Read Path where
  readPrec = fmap Path . lift $
    P.many $ do
      s <- PC.readPrec_to_P readPrec 0
      P.optional . P.char $ ','
      return s

step :: (Int, Int) -> Step -> (Int, Int)
step (x, y) N = (x, y + 2)
step (x, y) NE = (x + 1, y + 1)
step (x, y) SE = (x + 1, y - 1)
step (x, y) S = (x, y - 2)
step (x, y) SW = (x - 1, y - 1)
step (x, y) NW = (x - 1, y + 1)

distance :: (Int, Int) -> Int
distance (x, y)
  | x' <= y' = x' + (y' - x') `div` 2
  | otherwise = x'
  where
    (x', y') = (abs x, abs y)

part1Solution :: Path -> Int
part1Solution = distance . foldl' step (0, 0) . pSteps

part2Solution :: Path -> Int
part2Solution = maximum . map distance . scanl' step (0, 0) . pSteps
