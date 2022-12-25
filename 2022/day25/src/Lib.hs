module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data SNAFUDigit = MinusTwo | MinusOne | Zero | One | Two deriving (Eq, Ord)

instance Show SNAFUDigit where
  show MinusTwo = "="
  show MinusOne = "-"
  show Zero = "0"
  show One = "1"
  show Two = "2"

instance Read SNAFUDigit where
  readPrec =
    ((lift . P.char) '=' >> return MinusTwo)
      PC.<++ ((lift . P.char) '-' >> return MinusOne)
      PC.<++ ((lift . P.char) '0' >> return Zero)
      PC.<++ ((lift . P.char) '1' >> return One)
      PC.<++ ((lift . P.char) '2' >> return Two)

snafuDigitToDec :: SNAFUDigit -> Int
snafuDigitToDec MinusTwo = -2
snafuDigitToDec MinusOne = -1
snafuDigitToDec Zero = 0
snafuDigitToDec One = 1
snafuDigitToDec Two = 2

newtype SNAFUNumber = SNAFU {unSNAFU :: [SNAFUDigit]} deriving (Eq, Ord)

instance Show SNAFUNumber where
  show = concatMap show . unSNAFU

instance Read SNAFUNumber where
  readPrec = fmap SNAFU . lift . P.many1 . PC.readPrec_to_P readPrec $ 0

snafuToDec :: SNAFUNumber -> Int
snafuToDec = doer 1 . reverse . unSNAFU
  where
    doer :: Int -> [SNAFUDigit] -> Int
    doer _ [] = 0
    doer n (d : ds) = n * snafuDigitToDec d + doer (n * 5) ds

snafuNeg :: SNAFUNumber -> SNAFUNumber
snafuNeg = SNAFU . map neg . unSNAFU
  where
    neg MinusTwo = Two
    neg MinusOne = One
    neg Zero = Zero
    neg One = MinusOne
    neg Two = MinusTwo

decToSNAFU :: Int -> SNAFUNumber
decToSNAFU = SNAFU . reverse . doer
  where
    digs = [MinusTwo, MinusOne, Zero, One, Two]
    doer :: Int -> [SNAFUDigit]
    doer x
      | x' < 5 = [digs !! x']
      | otherwise = digs !! mx : doer dx
      where
        x' = x + 2
        mx = x' `mod` 5
        dx = x' `div` 5

part1Solution :: [SNAFUNumber] -> String
part1Solution = show . decToSNAFU . sum . map snafuToDec

part2Solution :: [SNAFUNumber] -> String
part2Solution = show . length
