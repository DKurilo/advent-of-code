module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Reg = A | B | C | D

instance Read Reg where
  readPrec =
    ((lift . P.char) 'a' >> return A)
      PC.+++ ((lift . P.char) 'b' >> return B)
      PC.+++ ((lift . P.char) 'c' >> return C)
      PC.+++ ((lift . P.char) 'd' >> return D)

data Arg = Reg Reg | Val Int

instance Read Arg where
  readPrec = (Reg <$> readPrec) PC.+++ (Val <$> readPrec)

data Op = CPY Arg Reg | INC Reg | DEC Reg | JNZ Arg Arg

instance Read Op where
  readPrec =
    ( do
        lift . P.string $ "cpy "
        arg <- readPrec
        lift P.skipSpaces
        CPY arg <$> readPrec
    )
      PC.+++ ( do
                 lift . P.string $ "inc "
                 INC <$> readPrec
             )
      PC.+++ ( do
                 lift . P.string $ "dec "
                 DEC <$> readPrec
             )
      PC.+++ ( do
                 lift . P.string $ "jnz "
                 arg <- readPrec
                 lift P.skipSpaces
                 JNZ arg <$> readPrec
             )

data Program = Program Int Int Int Int Int [Op]

getOps :: Program -> [Op]
getOps (Program _ _ _ _ _ ops) = ops

getReg :: Reg -> Program -> Int
getReg A (Program a _ _ _ _ _) = a
getReg B (Program _ b _ _ _ _) = b
getReg C (Program _ _ c _ _ _) = c
getReg D (Program _ _ _ d _ _) = d

setReg :: Reg -> Int -> Program -> Program
setReg A a (Program _ b c d step ops) = Program a b c d step ops
setReg B b (Program a _ c d step ops) = Program a b c d step ops
setReg C c (Program a b _ d step ops) = Program a b c d step ops
setReg D d (Program a b c _ step ops) = Program a b c d step ops

getArgValue :: Arg -> Program -> Int
getArgValue (Reg r) pr = getReg r pr
getArgValue (Val n) _ = n

getCurrentStep :: Program -> Int
getCurrentStep (Program _ _ _ _ step _) = step

getCurrentOp :: Program -> Op
getCurrentOp (Program _ _ _ _ step ops) = ops !! step

changeStep :: Int -> Program -> Program
changeStep step' (Program a b c d step ops) = Program a b c d (step + step') ops

nextStep :: Program -> Program
nextStep = changeStep 1

mkProgram :: [Op] -> Program
mkProgram = Program 0 0 0 0 0

tick :: Program -> Program
tick pr = case getCurrentOp pr of
  CPY arg reg -> nextStep . setReg reg (getArgValue arg pr) $ pr
  INC reg -> nextStep . setReg reg (getReg reg pr + 1) $ pr
  DEC reg -> nextStep . setReg reg (getReg reg pr - 1) $ pr
  JNZ arg1 arg2
    | getArgValue arg1 pr /= 0 -> changeStep (getArgValue arg2 pr) pr
    | otherwise -> nextStep pr

runProgram :: Program -> Program
runProgram pr
  | step < 0 || step >= (length . getOps) pr = pr
  | otherwise = runProgram . tick $ pr
  where
    step = getCurrentStep pr

part1Solution :: [Op] -> Int
part1Solution = getReg A . runProgram . mkProgram

part2Solution :: [Op] -> Int
part2Solution = getReg A . runProgram . setReg C 1 . mkProgram
