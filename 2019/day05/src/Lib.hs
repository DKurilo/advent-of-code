module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Sequence (Seq (..), fromList, index, update)
import qualified Data.Sequence as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

parse :: String -> Seq Int
parse cs = fromList . read $ ("[" ++ cs ++ "]")

data Status = Running | Halted | Error deriving (Show, Eq)

data Program = Program
  { pMem :: Seq Int,
    pIn :: [Int],
    pOut :: [Int],
    pPointer :: Int,
    pStatus :: Status
  }
  deriving (Show)

instance Read Program where
  readPrec = do
    mem <- fmap fromList . lift . P.many1 $ do
      n <- PC.readPrec_to_P readPrec 0
      P.optional . P.char $ ','
      return n
    return $ Program mem [] [] 0 Running

setInput :: [Int] -> Program -> Program
setInput ns p = p {pIn = ns}

setMem :: Int -> Int -> Program -> Program
setMem addr n p = p {pMem = update addr n . pMem $ p}

getMem :: Int -> Program -> Int
getMem addr = (`index` addr) . pMem

getModVal :: Int -> Int -> Program -> Int
getModVal m n p
  | m == 0 = getMem (getMem n p) p
  | m == 1 = getMem n p
  | otherwise = n

getFromInput :: Program -> (Int, Program)
getFromInput p = (n, p {pIn = ns})
  where
    (n : ns) = pIn p

output :: Int -> Program -> Program
output n p = p {pOut = n : pOut p}

goto :: Int -> Program -> Program
goto i p
  | i >= (S.length . pMem) p = p {pStatus = Halted}
  | otherwise = p {pPointer = i}

nextOp :: Int -> Program -> Program
nextOp i p
  | pPointer p + i >= (S.length . pMem) p = p {pStatus = Halted}
  | otherwise = p {pPointer = pPointer p + i}

binary :: (Int -> Int -> Int) -> Int -> Int -> Int -> Program -> Program
binary op i m1 m2 p = setMem r3 (v1 `op` v2) p
  where
    v1 = getModVal m1 (i + 1) p
    v2 = getModVal m2 (i + 2) p
    r3 = getMem (i + 3) p

applyOp :: Program -> Program
applyOp p
  | op == 1 = nextOp 4 . binary (+) i m1 m2 $ p
  | op == 2 = nextOp 4 . binary (*) i m1 m2 $ p
  | op == 3 = nextOp 2 . setMem (getMem (i + 1) p') inp $ p'
  | op == 4 = nextOp 2 . output v1 $ p
  | op == 5 = if v1 /= 0 then goto v2 p else nextOp 3 p
  | op == 6 = if v1 == 0 then goto v2 p else nextOp 3 p
  | op == 7 = nextOp 4 . setMem (getMem (i + 3) p) (if v1 < v2 then 1 else 0) $ p
  | op == 8 = nextOp 4 . setMem (getMem (i + 3) p) (if v1 == v2 then 1 else 0) $ p
  | otherwise = p {pStatus = Error}
  where
    i = pPointer p
    n = getMem (pPointer p) p
    op = n `mod` 100
    m1 = n `div` 100 `mod` 10
    m2 = n `div` 1000 `mod` 10
    m3 = n `div` 10000 `mod` 10
    v1 = getModVal m1 (i + 1) p
    v2 = getModVal m2 (i + 2) p
    (inp, p') = getFromInput p

execute :: Program -> Program
execute p
  | pStatus p /= Running = p
  | otherwise = execute . applyOp $ p

part1Solution :: Program -> Int
part1Solution = head . pOut . execute . setInput [1]

part2Solution :: Program -> Int
part2Solution = head . pOut . execute . setInput [5]
