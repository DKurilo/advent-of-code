module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data CMem = Ch Char | CHex String | CEsc Char deriving (Show)

instance Read CMem where
  readPrec = do
    ( do
        (lift . P.string) "\\x"
        CHex
          <$> lift
            ( do
                c1 <- P.get
                c2 <- P.get
                return [c1, c2]
            )
      )
      PC.<++ ( do
                 (lift . P.char) '\\'
                 CEsc <$> lift P.get
             )
      PC.<++ (Ch <$> lift P.get)

newtype SMem = SMem [CMem] deriving (Show)

instance Read SMem where
  readPrec = do
    (lift . P.char) '"'
    cs <- (lift . P.many) (PC.readPrec_to_P readPrec 0)
    (lift . P.char) '"'
    return . SMem $ cs

lengthSMem (SMem cs) = length cs

eval :: String -> Int
eval cs = length cs - (lengthSMem . read) cs

part1Solution :: [String] -> Int
part1Solution = sum . map eval

eval2 :: String -> Int
eval2 cs = (foldl doer 6 . (\(SMem cms) -> cms) . read) cs - length cs
  where
    doer :: Int -> CMem -> Int
    doer n (Ch _) = n + 1
    doer n (CHex _) = n + 5
    doer n (CEsc _) = n + 4

part2Solution :: [String] -> Int
part2Solution = sum . map eval2
