module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (first)
import Data.Bits ((.&.), (.|.))
import Data.List (foldl', inits, scanl', subsequences)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data OpCode
  = AddR
  | AddI
  | MulR
  | MulI
  | BanR
  | BanI
  | BorR
  | BorI
  | SetR
  | SetI
  | GtIR
  | GtRI
  | GtRR
  | EqIR
  | EqRI
  | EqRR
  deriving (Show, Eq)

instance Read OpCode where
  readPrec =
    lift $
      (AddR <$ P.string "addr")
        P.+++ (AddI <$ P.string "addi")
        P.+++ (MulR <$ P.string "mulr")
        P.+++ (MulI <$ P.string "muli")
        P.+++ (BanR <$ P.string "banr")
        P.+++ (BanI <$ P.string "bani")
        P.+++ (BorR <$ P.string "borr")
        P.+++ (BorI <$ P.string "bori")
        P.+++ (SetR <$ P.string "setr")
        P.+++ (SetI <$ P.string "seti")
        P.+++ (GtIR <$ P.string "gtir")
        P.+++ (GtRI <$ P.string "gtri")
        P.+++ (GtRR <$ P.string "gtrr")
        P.+++ (EqIR <$ P.string "eqir")
        P.+++ (EqRI <$ P.string "eqri")
        P.+++ (EqRR <$ P.string "eqrr")

allOpCodes :: [OpCode]
allOpCodes = [AddR, AddI, MulR, MulI, BanR, BanI, BorR, BorI, SetR, SetI, GtIR, GtRI, GtRR, EqIR, EqRI, EqRR]

data Op = Op {oOpCode :: OpCode, oA :: Int, oB :: Int, oC :: Int} deriving (Show, Eq)

instance Read Op where
  readPrec = do
    opCode <- readPrec
    lift P.skipSpaces
    a <- readPrec
    lift P.skipSpaces
    b <- readPrec
    lift P.skipSpaces
    Op opCode a b <$> readPrec

data Program = Program
  { pOps :: [Op],
    pPointer :: Int,
    pPointerRg :: Int,
    pRg0 :: Int,
    pRg1 :: Int,
    pRg2 :: Int,
    pRg3 :: Int,
    pRg4 :: Int,
    pRg5 :: Int
  }
  deriving (Eq, Show)

instance Read Program where
  readPrec = do
    lift . P.string $ "#ip "
    pointerReg <- readPrec
    lift P.skipSpaces
    ops <- lift . P.many1 $ do
      op <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      return op
    return $ Program ops 0 pointerReg 0 0 0 0 0 0

getReg :: Int -> Program -> Int
getReg 0 = pRg0
getReg 1 = pRg1
getReg 2 = pRg2
getReg 3 = pRg3
getReg 4 = pRg4
getReg 5 = pRg5
getReg _ = const 0

setReg :: Int -> Int -> Program -> Program
setReg 0 value pr = pr {pRg0 = value}
setReg 1 value pr = pr {pRg1 = value}
setReg 2 value pr = pr {pRg2 = value}
setReg 3 value pr = pr {pRg3 = value}
setReg 4 value pr = pr {pRg4 = value}
setReg 5 value pr = pr {pRg5 = value}
setReg _ _ pr = pr

setPointerFromReg :: Program -> Program
setPointerFromReg pr = pr {pPointer = getReg (pPointerRg pr) pr}

setPointer :: Int -> Program -> Program
setPointer i pr = setReg (pPointerRg pr) i (pr {pPointer = i})

nextOp :: Program -> Program
nextOp pr = setPointer (pPointer pr + 1) pr

applyROp :: (Int -> Int -> Int) -> Int -> Int -> Int -> Program -> Program
applyROp op a b c pr
  | c /= pPointerRg pr = nextOp pr'
  | otherwise = nextOp . setPointerFromReg $ pr'
  where
    pr' = setReg c (getReg a pr `op` getReg b pr) pr

applyIOp :: (Int -> Int -> Int) -> Int -> Int -> Int -> Program -> Program
applyIOp op a b c pr
  | c /= pPointerRg pr = nextOp pr'
  | otherwise = nextOp . setPointerFromReg $ pr'
  where
    pr' = setReg c (getReg a pr `op` b) pr

applyOp :: Op -> Program -> Program
applyOp (Op AddR a b c) = applyROp (+) a b c
applyOp (Op AddI a b c) = applyIOp (+) a b c
applyOp (Op MulR a b c) = applyROp (*) a b c
applyOp (Op MulI a b c) = applyIOp (*) a b c
applyOp (Op BanR a b c) = applyROp (.&.) a b c
applyOp (Op BanI a b c) = applyIOp (.&.) a b c
applyOp (Op BorR a b c) = applyROp (.|.) a b c
applyOp (Op BorI a b c) = applyIOp (.|.) a b c
applyOp (Op SetR a b c) = applyROp const a b c
applyOp (Op SetI a b c) = applyIOp (const id) b a c
applyOp (Op GtIR a b c) = applyIOp (\x1 x2 -> if x2 > x1 then 1 else 0) b a c
applyOp (Op GtRI a b c) = applyIOp (\x1 x2 -> if x1 > x2 then 1 else 0) a b c
applyOp (Op GtRR a b c) = applyROp (\x1 x2 -> if x1 > x2 then 1 else 0) a b c
applyOp (Op EqIR a b c) = applyIOp (\x1 x2 -> if x2 == x1 then 1 else 0) b a c
applyOp (Op EqRI a b c) = applyIOp (\x1 x2 -> if x1 == x2 then 1 else 0) a b c
applyOp (Op EqRR a b c) = applyROp (\x1 x2 -> if x1 == x2 then 1 else 0) a b c

execute :: Program -> (Program, Int)
execute pr
  | pPointer pr == 28 = (pr', i + 1)
  | pPointer pr >= 0 && pPointer pr < (length . pOps) pr = (pr', i + 1)
  | otherwise = (pr, 0)
  where
    (pr', i) = execute . applyOp (pOps pr !! pPointer pr) $ pr

executeTo :: Int -> Program -> (Program, Int)
executeTo n pr
  | pPointer pr == n = (pr, 0)
  | pPointer pr >= 0 && pPointer pr < (length . pOps) pr = (pr', i + 1)
  | otherwise = (pr, 0)
  where
    (pr', i) = executeTo n . applyOp (pOps pr !! pPointer pr) $ pr

part1Solution :: Program -> (Int, Int)
part1Solution = first (getReg 5) . executeTo 28

emul :: [Int]
emul = doer 0
  where
    doer f = doer' (f .|. 65536) 7571367
      where
        doer' c f'
          | 256 > c = f'' : doer f''
          | otherwise = doer' (if c' < 0 then 0 else c') f''
          where
            c' = c `div` 256
            f'' = (((f' + (c .&. 255)) .&. 16777215) * 65899) .&. 16777215

getLastUnique :: Int -> Bool
getLastUnique n = last ns `notElem` init ns
  where
    ns = take n emul

binSerch :: Int -> Int -> Int
binSerch minN maxN
  | minN == maxN = minN
  | maxN - minN == 1 = if getLastUnique maxN then maxN else minN
  | getLastUnique middleN = binSerch middleN maxN
  | otherwise = binSerch minN middleN
  where
    middleN = (maxN + minN) `div` 2

part2Solution :: Program -> Int
part2Solution pr = emul !! (binSerch 10000 60000000 - 1)
