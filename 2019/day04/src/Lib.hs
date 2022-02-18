module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl')
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Range = Range {rMin :: Int, rMax :: Int} deriving (Show)

instance Read Range where
  readPrec = do
    n1 <- readPrec
    lift . P.char $ '-'
    Range n1 <$> readPrec

stringify :: Int -> String
stringify 0 = ""
stringify n = stringify (n `div` 10) ++ show (n `mod` 10)

hasDuplicates :: String -> Bool
hasDuplicates cs = any (uncurry (==)) . zip cs . tail $ cs

neverDecrease :: String -> Bool
neverDecrease cs = all (uncurry (<=)) . zip cs . tail $ cs

isPassword :: Int -> Bool
isPassword n = hasDuplicates cs && neverDecrease cs
  where
    cs = stringify n

hasStrictDuplicates :: String -> Bool
hasStrictDuplicates =
  fst
    . foldl'
      ( \(p, (c, n)) c' ->
          if c == c'
            then (p, (c, n + 1))
            else
              if n == 2
                then (True, (c', 1))
                else (p, (c', 1))
      )
      (False, ('!', 0))
    . (++ "*")

isPassword2 :: Int -> Bool
isPassword2 n = hasStrictDuplicates cs && neverDecrease cs
  where
    cs = stringify n

part1Solution :: Range -> Int
part1Solution r = length [n | n <- [rMin r .. rMax r], isPassword n]

part2Solution :: Range -> Int
part2Solution r = length [n | n <- [rMin r .. rMax r], isPassword2 n]
