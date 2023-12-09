module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Text.ParserCombinators.ReadP as P
import Text.Read

newtype History = History {unHistory :: [Int]} deriving (Show)

instance Read History where
  readPrec = fmap History . lift . P.many $ do
    n <- readPrec_to_P readPrec 0
    P.skipSpaces
    return n

predict :: [Int] -> Int
predict [0] = 0
predict [_] = error "No predictions"
predict ns
  | all (== 0) ns = 0
  | otherwise = last ns + (predict . zipWith (-) (tail ns)) ns

part1Solution :: [String] -> Int
part1Solution = sum . fmap (predict . unHistory . read)

part2Solution :: [String] -> Int
part2Solution = sum . fmap (predict . reverse . unHistory . read)
