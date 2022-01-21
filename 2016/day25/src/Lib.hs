module Lib
  ( part1Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Reg = A | B | C | D deriving (Show)

instance Read Reg where
  readPrec =
    ((lift . P.char) 'a' >> return A)
      PC.+++ ((lift . P.char) 'b' >> return B)
      PC.+++ ((lift . P.char) 'c' >> return C)
      PC.+++ ((lift . P.char) 'd' >> return D)

data Arg = Reg Reg | Val Int deriving (Show)

instance Read Arg where
  readPrec = (Reg <$> readPrec) PC.+++ (Val <$> readPrec)

data Op = CPY Arg Arg | INC Reg | DEC Reg | JNZ Arg Arg | TGL Reg | OUT Arg deriving (Show)

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
      PC.+++ ( do
                 lift . P.string $ "tgl "
                 TGL <$> readPrec
             )
      PC.+++ ( do
                 lift . P.string $ "out "
                 OUT <$> readPrec
             )

isOut :: Op -> Bool
isOut (OUT _) = True
isOut _ = False

data Program = Program Int Int Int Int Int [Op] deriving (Show)

getOps :: Program -> [Op]
getOps (Program _ _ _ _ _ ops) = ops

setOps :: [Op] -> Program -> Program
setOps ops (Program a b c d step _) = Program a b c d step ops

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

toggle :: Int -> Program -> Program
toggle shift pr
  | pos < 0 = pr
  | pos >= length ops = pr
  | otherwise = setOps (zipWith (\i op -> if i == pos then doer op else op) [0 ..] . getOps $ pr) pr
  where
    ops = getOps pr
    pos = getCurrentStep pr + shift
    doer :: Op -> Op
    doer (CPY arg1 arg2) = JNZ arg1 arg2
    doer (JNZ arg1 arg2) = CPY arg1 arg2
    doer (INC reg) = DEC reg
    doer (DEC reg) = INC reg
    doer (TGL reg) = INC reg
    doer (OUT arg) = OUT arg

tick :: Program -> Program
tick pr = case getCurrentOp pr of
  CPY arg (Val _) -> nextStep pr
  CPY arg (Reg reg) -> nextStep . setReg reg (getArgValue arg pr) $ pr
  INC reg -> nextStep . setReg reg (getReg reg pr + 1) $ pr
  DEC reg -> nextStep . setReg reg (getReg reg pr - 1) $ pr
  JNZ arg1 arg2
    | getArgValue arg1 pr /= 0 -> changeStep (getArgValue arg2 pr) pr
    | otherwise -> nextStep pr
  TGL reg -> nextStep . toggle (getReg reg pr) $ pr
  OUT arg -> nextStep . toggle (getArgValue arg pr) $ pr

runProgram :: Program -> Program
runProgram pr
  | step < 0 || step >= (length . getOps) pr = pr
  | (isOut . getCurrentOp) pr = nextStep pr
  | otherwise = runProgram . tick $ pr
  where
    step = getCurrentStep pr

-- actually, this program just doing such thing:
-- d = a + 362 * 7
-- repeat:
--   a = d
--   do:
--     b = a `mod` 2
--     a = a `div` 2
--     out b
--   loop while a > 0
-- so solution is number that is:
-- x0 = 2
-- x(n + 1) = (2 * xn + 1) * 2
-- x > 362 * 7
-- so we don't even need program here
part1Solution :: [Op] -> Int
part1Solution ops =
  fst
    . head
    . filter ((== (concat . replicate 20) [0, 1]) . snd)
    . map (\a -> (a, take 40 . drop 1 . map (getReg B) . iterate runProgram . setReg A a $ pr))
    $ [0 ..]
  where
    pr = mkProgram ops
