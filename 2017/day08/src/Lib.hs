module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Register = Register String deriving (Show, Eq, Ord)

instance Read Register where
  readPrec = Register <$> (lift . P.munch1) isAlpha

data Op = OLT | OEQ | OGT | OLE | OGE | ONE deriving (Show)

instance Read Op where
  readPrec =
    lift $
      (P.char '<' >> return OLT)
        P.+++ (P.string "==" >> return OEQ)
        P.+++ (P.char '>' >> return OGT)
        P.+++ (P.string "<=" >> return OLE)
        P.+++ (P.string ">=" >> return OGE)
        P.+++ (P.string "!=" >> return ONE)

data If = If {ifOp :: Op, ifReg :: Register, ifVal :: Int} deriving (Show)

instance Read If where
  readPrec = do
    lift . P.string $ "if"
    lift P.skipSpaces
    reg <- readPrec
    lift P.skipSpaces
    op <- readPrec
    lift P.skipSpaces
    If op reg <$> readPrec

data StType = Inc | Dec deriving (Show)

instance Read StType where
  readPrec = lift $ (P.string "inc" >> return Inc) P.+++ (P.string "dec" >> return Dec)

data Statement = Statement {stType :: StType, stReg :: Register, stVal :: Int, stIf :: If} deriving (Show)

instance Read Statement where
  readPrec = do
    reg <- readPrec
    lift P.skipSpaces
    t <- readPrec
    lift P.skipSpaces
    val <- readPrec
    lift P.skipSpaces
    Statement t reg val <$> readPrec

data Program = Program {pStats :: [Statement], pPos :: Int, pRegs :: M.Map Register Int, pMax :: Maybe Int} deriving (Show)

mkProgram :: [Statement] -> Program
mkProgram sts = Program sts 0 M.empty Nothing

nextPos :: Program -> Program
nextPos p = p {pPos = pPos p + 1}

getReg :: Register -> Program -> Int
getReg reg = fromMaybe 0 . (reg `M.lookup`) . pRegs

setReg :: Register -> Int -> Program -> Program
setReg reg val p = p {pRegs = (M.insert reg val . pRegs) p, pMax = max (pMax p) (Just val)}

op :: Op -> Int -> Int -> Bool
op OLT x y = x < y
op OEQ x y = x == y
op OGT x y = x > y
op OLE x y = x <= y
op OGE x y = x >= y
op ONE x y = x /= y

action :: StType -> Int -> Int -> Int
action Inc x y = x + y
action Dec x y = x - y

checkIf :: If -> Program -> Bool
checkIf i p = op (ifOp i) (getReg (ifReg i) p) (ifVal i)

applyCurrentStatement :: Program -> Program
applyCurrentStatement p
  | checkIf (stIf st) p = setReg (stReg st) (action (stType st) (getReg (stReg st) p) (stVal st)) p
  | otherwise = p
  where
    st = pStats p !! pPos p

runProgram :: Program -> Program
runProgram p
  | pPos p >= (length . pStats) p = p
  | otherwise = (runProgram . nextPos . applyCurrentStatement) p

part1Solution :: [Statement] -> Int
part1Solution = maximum . M.elems . pRegs . runProgram . mkProgram

part2Solution :: [Statement] -> Maybe Int
part2Solution = pMax . runProgram . mkProgram
