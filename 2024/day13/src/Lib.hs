module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

costA :: Int
costA = 3

costB :: Int
costB = 1

maxUse :: Int
maxUse = 100

data Button = Button
  { bx :: Int
  , by :: Int
  } deriving (Show)

readSign :: P.ReadP Int
readSign =
  (do
     _ <- P.char '+'
     return 1)
    P.<++ (do
             _ <- P.char '-'
             return (-1))

readNumb :: P.ReadP Int
readNumb = fmap read . P.munch1 $ isDigit

readSgnNumb :: P.ReadP Int
readSgnNumb = do
  sgn <- readSign
  (sgn *) <$> readNumb

instance Read Button where
  readPrec =
    lift $ do
      _ <- P.string "Button"
      P.skipSpaces
      _ <- P.char 'A' P.+++ P.char 'B'
      _ <- P.char ':'
      P.skipSpaces
      _ <- P.char 'X'
      x <- readSgnNumb
      _ <- P.char ','
      P.skipSpaces
      _ <- P.char 'Y'
      Button x <$> readSgnNumb

data Prize = Prize
  { prx :: Int
  , pry :: Int
  } deriving (Show)

instance Read Prize where
  readPrec =
    lift $ do
      _ <- P.string "Prize:"
      P.skipSpaces
      _ <- P.string "X="
      x <- readNumb
      _ <- P.char ','
      P.skipSpaces
      _ <- P.string "Y="
      Prize x <$> readNumb

data Claw = Claw
  { cba :: Button
  , cbb :: Button
  , cpr :: Prize
  } deriving (Show)

instance Read Claw where
  readPrec = do
    ba <- readPrec
    lift P.skipSpaces
    bb <- readPrec
    lift P.skipSpaces
    Claw ba bb <$> readPrec

newtype Arcade = Arcade
  { unArcade :: [Claw]
  } deriving (Show)

readClaw :: P.ReadP Claw
readClaw = do
  c <- PC.readPrec_to_P readPrec 0
  P.skipSpaces
  return c

instance Read Arcade where
  readPrec = lift . fmap Arcade . P.many1 $ readClaw

-- xa * m + xb * n = xp
-- ya * m + yb * n = yp
bestCost1 :: Claw -> Maybe Int
bestCost1 c
  | null costs = Nothing
  | otherwise = Just . minimum $ costs
  where
    costs =
      [ m * costA + n * costB
      | m <- [0 .. maxUse]
      , n <- [0 .. maxUse]
      , m * (bx . cba) c + n * (bx . cbb) c == (prx . cpr) c
      , m * (by . cba) c + n * (by . cbb) c == (pry . cpr) c
      ]

part1Solution :: String -> Int
part1Solution = sum . fmap (fromMaybe 0 . bestCost1) . unArcade . read

-- xa * m + xb * n = xp
-- ya * m + yb * n = yp
-- m = (xp - xb * n) / xa
-- ya * xp / xa - ya * xb * n / xa + yb * n = yp
-- n * (yb - ya * xb / xa) = yp - ya * xp / xa
-- n = (yp * xa - ya * xp) / (yb * xa - ya * xb)
--
-- 1 * m + 3 * n = 9
-- 1 * m + 1 * n = 3
-- n = (3 * 1 - 1 * 9) / (1 - 3) = 3
-- m = (9 - 3 * 3) / 1 = 0
--
-- Worst can be in case we can achieve x and y in multiple ways
-- trivial example is
-- xp = 100, xa = 5, xb = 10
-- yp = 100, ya = 5, yb = 10
-- and non-trivial:
-- xp = 312, xa = 3, xb = 6
-- yp = 520, ya = 5, yb = 10
-- but it looks like there is no such cases in my input.
bestCost2 :: Claw -> Maybe Int
bestCost2 c
  | d2n /= 0 =
    if d1n `mod` d2n == 0 && d1m `mod` xa == 0
      then Just (costA * m + costB * n)
      else Nothing
  | otherwise = error "oops"
  where
    ba = cba c
    xa = bx ba
    ya = by ba
    bb = cbb c
    xb = bx bb
    yb = by bb
    pr = cpr c
    xp = prx pr
    yp = pry pr
    d1n = yp * xa - ya * xp
    d2n = yb * xa - ya * xb
    n = d1n `div` d2n
    d1m = xp - xb * n
    m = d1m `div` xa

part2Solution :: String -> Int
part2Solution =
  sum
    . fmap
        ((fromMaybe 0 . bestCost2)
           . (\c ->
                c
                  { cpr =
                      (cpr c)
                        { prx = (prx . cpr) c + 10000000000000
                        , pry = (pry . cpr) c + 10000000000000
                        }
                  }))
    . unArcade
    . read
