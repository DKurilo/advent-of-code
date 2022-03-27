module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.List (foldl', subsequences)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Step = Reverse | Rotate Integer | Shuffle Integer deriving (Show)

instance Read Step where
  readPrec =
    lift $
      (P.string "deal into new stack" >> return Reverse)
        P.+++ (P.string "cut " >> fmap Rotate (PC.readPrec_to_P readPrec 0))
        P.+++ (P.string "deal with increment " >> fmap Shuffle (PC.readPrec_to_P readPrec 0))

data Deck = Deck {dSize :: Integer, dCard :: Integer, dSteps :: [Step]} deriving (Show)

instance Read Deck where
  readPrec = do
    size <- readPrec
    lift P.skipSpaces
    card <- readPrec
    lift P.skipSpaces
    steps <- lift . P.many1 $ do
      st <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      return st
    return $ Deck size card steps

applyStep :: Integer -> Integer -> Step -> Integer
applyStep size card Reverse = size - card - 1
applyStep size card (Rotate n) = (size - n + card) `mod` size
applyStep size card (Shuffle n) = (card * n) `mod` size

cardPos :: Deck -> Integer
cardPos d = foldl' (applyStep (dSize d)) (dCard d) . dSteps $ d

part1Solution :: Deck -> Integer
part1Solution = cardPos

powerMod :: Integral a => a -> a -> a -> a
powerMod b e m = x
  where
    (_, _, x) =
      until
        (\(_, e, _) -> e <= 0)
        ( \(b, e, x) ->
            ( mod (b * b) m,
              div e 2,
              if odd e
                then mod (b * x) m
                else x
            )
        )
        (b, e, 1)

getPositiveMod :: Integral a => a -> a -> a
getPositiveMod q n = head . filter (>= 0) . map (\l -> l * q + n) $ [0 ..]

modInverse :: Integral a => a -> a -> a
modInverse q 1 = 1
modInverse q p = (n * q + 1) `div` p
  where
    n = p - modInverse p (q `mod` p)

unApplyStep :: Integer -> Integer -> Step -> Integer
unApplyStep size pos Reverse = size - pos - 1
unApplyStep size pos (Rotate n) = (pos + n + size) `mod` size
unApplyStep size pos (Shuffle n) = getPositiveMod size (pos * modInverse size n) `mod` size

unApplySteps :: Integer -> Integer -> [Step] -> Integer
unApplySteps size pos = foldl' (unApplyStep size) pos . reverse

-- I wasn't able to solve it. Looks like lack of math and I need to learn something about linear modulo operations.
-- So I used this idea to solve it:
-- https://www.reddit.com/r/adventofcode/comments/ee0rqi/2019_day_22_solutions/fbnifwk/?utm_source=reddit&utm_medium=web2x&context=3
part2Solution :: Deck -> Integer
part2Solution d = itimes
  where
    deckSize = 119315717514047
    times = 101741582076661
    i0 = 2020
    i1 = unApplySteps deckSize i0 (dSteps d)
    i2 = unApplySteps deckSize i1 (dSteps d)
    i10 = getPositiveMod deckSize (i1 - i0)
    i21 = getPositiveMod deckSize (i2 - i1)
    a = (i21 * modInverse deckSize i10) `mod` deckSize
    b = getPositiveMod deckSize (i1 - a * i0)
    mulmod x y = (x * y) `mod` deckSize
    itimes =
      ((powerMod a times deckSize `mulmod` i0) + ((powerMod a times deckSize - 1) `mulmod` modInverse deckSize (a - 1) `mulmod` b))
        `mod` deckSize
