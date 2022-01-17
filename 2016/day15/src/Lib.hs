module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isDigit)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Disc = Disc Int Int deriving (Eq, Show)

instance Read Disc where
  readPrec = do
    lift . P.string $ "Disc #"
    lift . P.munch $ isDigit
    lift . P.string $ " has "
    positions <- readPrec
    lift . P.string $ " positions; at time=0, it is at position "
    position <- readPrec
    lift . P.char $ '.'
    return $ Disc positions position

isGoodTime :: Int -> [Disc] -> Bool
isGoodTime n = all (\(s, Disc ps p) -> (n + s + p) `mod` ps == 0) . zip [1 ..]

part1Solution :: [Disc] -> Int
part1Solution ds = head [n | n <- [0 ..], isGoodTime n ds]

part2Solution :: [Disc] -> Int
part2Solution ds = head [n | n <- [0 ..], isGoodTime n (ds ++ [Disc 11 0])]
