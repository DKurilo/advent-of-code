module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Data.List (foldl')
import Data.Map (Map (..), delete, empty, fromList, insert, size)
import qualified Data.Map as M
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Ingredient = Ingredient {unIngr :: String} deriving (Show, Eq, Ord)

instance Read Ingredient where
  readPrec = fmap Ingredient . lift . P.munch1 $ isAlpha

newtype Reactions = Reactions {unReact :: Map Ingredient (Int, [(Int, Ingredient)])} deriving (Show)

readIngrQuant = do
  n <- PC.readPrec_to_P readPrec 0
  P.skipSpaces
  i <- PC.readPrec_to_P readPrec 0
  P.optional $ do
    P.char ','
    P.skipSpaces
  return (n, i)

instance Read Reactions where
  readPrec = fmap (Reactions . fromList) . lift . P.many1 $ do
    is <- P.many1 readIngrQuant
    P.skipSpaces
    P.string "=>"
    P.skipSpaces
    (n, i) <- readIngrQuant
    P.skipSpaces
    return (i, (n, is))

divRoundUp :: Int -> Int -> Int
divRoundUp m n = m `div` n + (if m `mod` n > 0 then 1 else 0)

amountNeeded :: Ingredient -> Ingredient -> Int -> Map Ingredient Int -> Reactions -> (Int, Map Ingredient Int)
amountNeeded from what n leftovers rs
  | what == from = (n', leftovers')
  | n' == 0 = (n', leftovers')
  | otherwise = case what `M.lookup` unReact rs of
    Just (nWhat, is) ->
      foldl'
        ( \(res, los) (nFrom, i) ->
            let (res', los') = amountNeeded from i (nFrom * t) los rs
             in (res + res', if nWhat * t > n' then insert what (nWhat * t - n') los' else los')
        )
        (0, leftovers')
        is
      where
        t = n' `divRoundUp` nWhat
    _ -> error "oops, I can't do this"
  where
    (n', leftovers') = case what `M.lookup` leftovers of
      Just n''
        | n'' > n -> (0, insert what (n'' - n) leftovers)
        | otherwise -> (n - n'', delete what leftovers)
      _ -> (n, leftovers)

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch p from to
  | from == to = from
  | to - from == 1 && p to = to
  | to - from == 1 = from
  | p middle = binarySearch p middle to
  | otherwise = binarySearch p from middle
  where
    middle = (from + to) `div` 2

part1Solution :: Reactions -> Int
part1Solution = fst . amountNeeded (Ingredient "ORE") (Ingredient "FUEL") 1 empty

part2Solution :: Reactions -> Int
part2Solution rs = binarySearch isGood 0 1000000000000
  where
    isGood n = fst (amountNeeded (Ingredient "ORE") (Ingredient "FUEL") n empty rs) <= 1000000000000
