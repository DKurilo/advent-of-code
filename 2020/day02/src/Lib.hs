module Lib
    ( part1solution
    , part2solution
    ) where

import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

data Password = Pass Int Int Char String deriving (Show)

instance Read Password where
    readPrec = do
        n1 <- readPrec
        (lift . P.char) '-'
        n2 <- readPrec
        lift P.skipSpaces
        c <- lift P.get
        (lift . P.char) ':'
        lift P.skipSpaces
        Pass n1 n2 c <$> lift (P.many P.get)

checkPasswordPart1 :: Password -> Bool
checkPasswordPart1 (Pass minAmount maxAmount c cs) = amount >= minAmount && amount <= maxAmount
    where amount = (length . filter (==c)) cs

checkPasswordPart2 :: Password -> Bool
checkPasswordPart2 (Pass i1 i2 c cs) = check i1 /= check i2
    where check i = length cs >= i && cs !! (i - 1) == c

solution :: (Password -> Bool) -> IO ()
solution p = readFile "./input" >>= print . length . filter p . map read . filter (not . null) . lines

part1solution :: IO ()
part1solution = solution checkPasswordPart1

part2solution :: IO ()
part2solution = solution checkPasswordPart2
