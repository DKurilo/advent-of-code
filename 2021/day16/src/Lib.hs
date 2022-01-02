module Lib
  ( part1Solution,
    part2Solution,
    hexToBin,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

binToInt :: String -> Int
binToInt = foldl (\n c -> n * 2 + if c == '0' then 0 else 1) 0

-- https://hackage.haskell.org/package/utility-ht-0.0.16/docs/src/Text.Read.HT.html#readMany
readMany :: (Read a) => String -> [a]
readMany x =
  let contReadList [] = []
      contReadList [y] = fst y : readMany (snd y)
      contReadList _ = error "readMany: ambiguous parses"
   in contReadList (reads x)

type Version = Int

type Type = Int

type LengthType = Int

type Val = Int

type Length = Int

data Packet = Literal Version Type Val | Operator Version Type LengthType Length [Packet] deriving (Eq, Show)

instance Read Packet where
  readPrec = lift $ do
    let getN n = P.count n P.get
        getNInt n = binToInt <$> getN n
    v <- getNInt 3
    t <- getNInt 3
    if t == 4
      then
        Literal v t . binToInt . concat <$> do
          ns <- P.many $ do
            P.char '1'
            getN 4
          P.char '0'
          n <- getN 4
          return (ns ++ [n])
      else do
        lt <- getNInt 1
        l <- getNInt (if lt == 0 then 15 else 11)
        Operator v t lt l
          <$> if lt == 0
            then do
              cs <- getN l
              return . readMany $ cs
            else do
              P.count l (PC.readPrec_to_P readPrec 0)

newtype TransmittedMessage = TM Packet deriving (Show)

instance Read TransmittedMessage where
  readPrec = do
    p <- readPrec
    lift $ P.many $ P.char '0'
    return $ TM p

tmToP (TM p) = p

hexToBin :: Char -> String
hexToBin '0' = "0000"
hexToBin '1' = "0001"
hexToBin '2' = "0010"
hexToBin '3' = "0011"
hexToBin '4' = "0100"
hexToBin '5' = "0101"
hexToBin '6' = "0110"
hexToBin '7' = "0111"
hexToBin '8' = "1000"
hexToBin '9' = "1001"
hexToBin 'A' = "1010"
hexToBin 'B' = "1011"
hexToBin 'C' = "1100"
hexToBin 'D' = "1101"
hexToBin 'E' = "1110"
hexToBin 'F' = "1111"
hexToBin _ = ""

getVersionsSum :: Packet -> Int
getVersionsSum (Literal v _ _) = v
getVersionsSum (Operator v _ _ _ ps) = v + (sum . map getVersionsSum) ps

evaluatePackage :: Packet -> Int
evaluatePackage (Literal _ _ n) = n
evaluatePackage (Operator _ t _ _ ps)
  | t == 0 = sum vs
  | t == 1 = product vs
  | t == 2 = minimum vs
  | t == 3 = maximum vs
  | t == 5 = cond (>)
  | t == 6 = cond (<)
  | t == 7 = cond (==)
  where
    vs = map evaluatePackage ps
    [v1, v2] = vs
    cond p = if v1 `p` v2 then 1 else 0

part1Solution :: String -> Int
part1Solution = getVersionsSum . tmToP . read

part2Solution :: String -> Int
part2Solution = evaluatePackage . tmToP . read
