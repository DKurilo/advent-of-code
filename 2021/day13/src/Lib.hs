module Lib
  ( part1Solution,
    part2Solution,
    X (..),
    Y (..),
    Point (..),
    Paper (..),
    Fold (..),
  )
where

import Data.List (intercalate)
import Data.Set (Set (..), findMax, fromList, member, size, union)
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

type X = Int

type Y = Int

data Point = P X Y deriving (Eq, Ord, Show)

instance Read Point where
  readPrec = do
    x <- readPrec
    (lift . P.char) ','
    P x <$> readPrec

newtype Paper = Paper (Set Point) deriving (Eq, Ord)

instance Show Paper where
  show (Paper p) = intercalate "\n" [[if P x y `member` p then '#' else '.' | x <- [0 .. maxX]] | y <- [0 .. maxY]]
    where
      maxX = findMax . S.map (\(P x _) -> x) $ p
      maxY = findMax . S.map (\(P _ y) -> y) $ p

data Fold = X X | Y Y

instance Read Fold where
  readPrec = do
    (lift . P.string) "fold along "
    lift $
      ( do
          P.string "y="
          Y <$> PC.readPrec_to_P readPrec 0
      )
        P.+++ ( do
                  P.string "x="
                  X <$> PC.readPrec_to_P readPrec 0
              )

folds :: [Fold] -> Paper -> [Paper]
folds fs paper = scanl fold1Time paper fs
  where
    fold1Time (Paper p) f = Paper $ p1 `union` p2
      where
        p1 = case f of
          X x -> S.filter (\(P px _) -> px < x) p
          Y y -> S.filter (\(P _ py) -> py < y) p
        p2 = case f of
          X x -> S.map (\(P px py) -> P (2 * x - px) py) . S.filter (\(P px _) -> px > x) $ p
          Y y -> S.map (\(P px py) -> P px (2 * y - py)) . S.filter (\(P _ py) -> py > y) $ p

part1Solution :: [Fold] -> Paper -> Int
part1Solution fs = size . (\(Paper p) -> p) . (!! 1) . folds fs

part2Solution :: [Fold] -> Paper -> String
part2Solution fs = show . last . folds fs
