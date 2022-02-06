{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bits ((.&.), (.|.))
import Data.List (foldl')
import qualified Data.Map as M
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

allOpCodes :: [OpCode]
allOpCodes = [AddR, AddI, MulR, MulI, BanR, BanI, BorR, BorI, SetR, SetI, GtIR, GtRI, GtRR, EqIR, EqRI, EqRR]

data Regs = Regs {rg0 :: Int, rg1 :: Int, rg2 :: Int, rg3 :: Int} deriving (Eq, Show)

getReg :: Int -> Regs -> Int
getReg 0 = rg0
getReg 1 = rg1
getReg 2 = rg2
getReg 3 = rg3
getReg _ = const 0

setReg :: Int -> Int -> Regs -> Regs
setReg 0 value rs = rs {rg0 = value}
setReg 1 value rs = rs {rg1 = value}
setReg 2 value rs = rs {rg2 = value}
setReg 3 value rs = rs {rg3 = value}
setReg _ _ rs = rs

instance Read Regs where
  readPrec = do
    [r0, r1, r2, r3] <- readPrec
    return $ Regs r0 r1 r2 r3

data Instr = Instr {iCode :: Int, iA :: Int, iB :: Int, iC :: Int} deriving (Show)

instance Read Instr where
  readPrec = do
    code <- readPrec
    lift P.skipSpaces
    a <- readPrec
    lift P.skipSpaces
    b <- readPrec
    lift P.skipSpaces
    Instr code a b <$> readPrec

data Test = Test {tBefore :: Regs, tInstr :: Instr, tAfter :: Regs} deriving (Show)

instance Read Test where
  readPrec = do
    lift . P.string $ "Before: "
    before <- readPrec
    lift P.skipSpaces
    instr <- readPrec
    lift P.skipSpaces
    lift . P.string $ "After: "
    Test before instr <$> readPrec

data Input = Input {inTests :: [Test], inInstrs :: [Instr]} deriving (Show)

instance Read Input where
  readPrec = do
    tests <- lift . P.many $ do
      test <- PC.readPrec_to_P readPrec 0
      P.count 2 (P.char '\n')
      return test
    lift $ P.count 2 (P.char '\n')
    instrs <- lift . P.many $ PC.readPrec_to_P readPrec 0
    return $ Input tests instrs

data Op = Op {oOpCode :: OpCode, oA :: Int, oB :: Int, oC :: Int} deriving (Show)

instrToOp :: OpCode -> Instr -> Op
instrToOp oc i = Op oc (iA i) (iB i) (iC i)

applyROp :: (Int -> Int -> Int) -> Int -> Int -> Int -> Regs -> Regs
applyROp op a b c rs = setReg c (getReg a rs `op` getReg b rs) rs

applyIOp :: (Int -> Int -> Int) -> Int -> Int -> Int -> Regs -> Regs
applyIOp op a b c rs = setReg c (getReg a rs `op` b) rs

applyOp :: Op -> Regs -> Regs
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

infer :: Test -> [OpCode] -> [OpCode]
infer t = map fst . filter snd . map (\oc -> (oc, applyOp (instrToOp oc (tInstr t)) (tBefore t) == tAfter t))

part1Solution :: Input -> Int
part1Solution = length . filter (>= 3) . map (\t -> length (infer t allOpCodes)) . inTests

execute :: [Op] -> Regs -> Regs
execute os regs = foldl' (flip applyOp) regs os

part2Solution :: Input -> Int
part2Solution inp = getReg 0 . execute ops $ Regs 0 0 0 0
  where
    initIoMap = M.fromList . map (,allOpCodes) $ [0 .. 15]
    ioMap =
      foldl'
        ( \m t ->
            let i = iCode . tInstr $ t
             in case M.lookup i m of
                  Just ocs -> case infer t ocs of
                    [oc] -> M.mapWithKey (\k ocs' -> if k == i then ocs' else filter (/= oc) ocs') . M.insert i [oc] $ m
                    ocs' -> M.insert i ocs' m
                  _ -> error "unknown instruction!"
        )
        initIoMap
        . inTests
        $ inp
    ops =
      map
        ( \i -> case M.lookup (iCode i) ioMap of
            Just [oc] -> instrToOp oc i
            _ -> error "some instruction wasn't infered"
        )
        . inInstrs
        $ inp
