module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (sort)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Signal = List [Signal] | Val Int deriving (Show, Eq)

instance Read Signal where
  readPrec =
    ( do
        _ <- lift . P.char $ '['
        signals <- lift . P.many $ do
          s <- PC.readPrec_to_P readPrec 0
          _ <- P.optional . P.char $ ','
          return s
        _ <- lift . P.char $ ']'
        return . List $ signals
    )
      PC.<++ (Val <$> readPrec)

instance Ord Signal where
  compare (Val n1) (Val n2)
    | n1 < n2 = LT
    | n1 == n2 = EQ
    | otherwise = GT
  compare v1@(Val _) v2@(List _) = compare (List [v1]) v2
  compare v1@(List _) v2@(Val _) = compare v1 (List [v2])
  compare (List sigs1) (List sigs2)
    | null sigs1 && null sigs2 = EQ
    | null sigs1 = LT
    | null sigs2 = GT
    | otherwise = case compare (head sigs1) (head sigs2) of
      GT -> GT
      LT -> LT
      _ -> compare (List . tail $ sigs1) (List . tail $ sigs2)

data SPair = SPair Signal Signal deriving (Show)

instance Read SPair where
  readPrec = do
    lift P.skipSpaces
    p1 <- readPrec
    lift P.skipSpaces
    p2 <- readPrec
    lift P.skipSpaces
    return $ SPair p1 p2

newtype SPairs = SPairs {unSPairs :: [SPair]} deriving (Show)

instance Read SPairs where
  readPrec = SPairs <$> (lift . P.many . PC.readPrec_to_P readPrec $ 0)

isRightOrder :: SPair -> Bool
isRightOrder (SPair s1 s2) = s1 <= s2

collectSPairs :: SPairs -> [Signal]
collectSPairs = concatMap (\(SPair s1 s2) -> [s1, s2]) . unSPairs

part1Solution :: SPairs -> Int
part1Solution = sum . map fst . filter snd . zip [1 ..] . map isRightOrder . unSPairs

part2Solution :: SPairs -> Int
part2Solution = product . map fst . filter (\(_, s) -> s == div2 || s == div6) . zip [1 ..] . sort . addDividers . collectSPairs
  where
    div2 = List [List [Val 2]]
    div6 = List [List [Val 6]]
    addDividers ps = [div2, div6] <> ps
