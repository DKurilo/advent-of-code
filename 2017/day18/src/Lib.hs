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

newtype Register = Reg {regName :: String} deriving (Eq, Ord, Show)

instance Read Register where
  readPrec = fmap Reg . lift . P.munch1 $ isAlpha

data Argument = AVal Int | AReg Register deriving (Show)

instance Read Argument where
  readPrec = (AVal <$> readPrec) PC.<++ (AReg <$> readPrec)

data Op
  = Snd Argument
  | Send Argument
  | Set Register Argument
  | Add Register Argument
  | Mul Register Argument
  | Mod Register Argument
  | Rcv Register
  | Recv Register
  | Jgz Argument Argument
  deriving (Show)

instance Read Op where
  readPrec =
    ( do
        lift . P.string $ "snd"
        lift P.skipSpaces
        Snd <$> readPrec
    )
      PC.<++ ( do
                 lift . P.string $ "set"
                 lift P.skipSpaces
                 r <- readPrec
                 lift P.skipSpaces
                 Set r <$> readPrec
             )
      PC.<++ ( do
                 lift . P.string $ "add"
                 lift P.skipSpaces
                 r <- readPrec
                 lift P.skipSpaces
                 Add r <$> readPrec
             )
      PC.<++ ( do
                 lift . P.string $ "mul"
                 lift P.skipSpaces
                 r <- readPrec
                 lift P.skipSpaces
                 Mul r <$> readPrec
             )
      PC.<++ ( do
                 lift . P.string $ "mod"
                 lift P.skipSpaces
                 r <- readPrec
                 lift P.skipSpaces
                 Mod r <$> readPrec
             )
      PC.<++ ( do
                 lift . P.string $ "rcv"
                 lift P.skipSpaces
                 Rcv <$> readPrec
             )
      PC.<++ ( do
                 lift . P.string $ "jgz"
                 lift P.skipSpaces
                 r <- readPrec
                 lift P.skipSpaces
                 Jgz r <$> readPrec
             )

data StopFlag = Wait | Exit | Running deriving (Eq, Show)

data Program = Program
  { prOps :: [Op],
    prPos :: Int,
    prCtx :: M.Map Register Int,
    prSnd :: Int,
    prSentCnt :: Int,
    prToSend :: [Int],
    prToRecv :: [Int],
    prFlag :: StopFlag
  }
  deriving (Show)

mkProgram :: [Op] -> Program
mkProgram ops = Program ops 0 M.empty 0 0 [] [] Running

regVal :: Register -> Program -> Int
regVal r = fromMaybe 0 . M.lookup r . prCtx

argVal :: Argument -> Program -> Int
argVal (AVal x) _ = x
argVal (AReg r) pr = regVal r pr

jump :: Int -> Program -> Program
jump n pr = pr {prPos = n + prPos pr}

next :: Program -> Program
next = jump 1

setReg :: Register -> Int -> Program -> Program
setReg reg x pr = pr {prCtx = M.insert reg x . prCtx $ pr}

applyBin :: Register -> (Int -> Int -> Int) -> Argument -> Program -> Program
applyBin reg op arg pr = next . setReg reg (regVal reg pr `op` argVal arg pr) $ pr

applyOp :: Op -> Program -> Program
applyOp (Snd arg) pr = next $ pr {prSnd = argVal arg pr}
applyOp (Set reg arg) pr = applyBin reg (\_ x -> x) arg pr
applyOp (Add reg arg) pr = applyBin reg (+) arg pr
applyOp (Mul reg arg) pr = applyBin reg (*) arg pr
applyOp (Mod reg arg) pr = applyBin reg mod arg pr
applyOp (Rcv reg) pr
  | regVal reg pr /= 0 = next $ setReg reg (prSnd pr) pr
  | otherwise = next pr
applyOp (Jgz x y) pr
  | argVal x pr > 0 = jump (argVal y pr) pr
  | otherwise = next pr
applyOp (Send arg) pr = next $ pr {prToSend = argVal arg pr : prToSend pr, prSentCnt = 1 + prSentCnt pr}
applyOp (Recv reg) pr
  | (null . prToRecv) pr = pr {prFlag = Wait}
  | otherwise = next . setReg reg (head . prToRecv $ pr) $ pr {prToRecv = tail . prToRecv $ pr}

runProgramUntil :: (Program -> Bool) -> Program -> Program
runProgramUntil f pr
  | pos < 0 || pos >= length ops = pr {prFlag = Exit}
  | f pr = pr
  | prFlag pr /= Running = pr
  | otherwise = runProgramUntil f . applyOp (ops !! pos) $ pr
  where
    pos = prPos pr
    ops = prOps pr

isRecoverExecuted :: Program -> Bool
isRecoverExecuted pr = isRcv (prOps pr !! prPos pr)
  where
    isRcv :: Op -> Bool
    isRcv (Rcv reg) = regVal reg pr /= 0
    isRcv _ = False

data System = System {sysPr0 :: Program, sysPr1 :: Program} deriving (Show)

mkSystem :: [Op] -> System
mkSystem ops = System pr0 pr1
  where
    pr0 = setReg (Reg "p") 0 . fixProgram . mkProgram $ ops
    pr1 = setReg (Reg "p") 1 . fixProgram . mkProgram $ ops

clearQueueS :: Program -> Program
clearQueueS pr = pr {prToSend = []}

setQueueR :: [Int] -> Program -> Program
setQueueR xs pr = pr {prToRecv = xs, prFlag = Running}

runSystem :: System -> System
runSystem s
  | flag0 == Exit && flag1 == Exit = s -- normal termination
  | flag0 == Wait && (not . null) qs1 = runSystem $ s {sysPr0 = setQueueR qs1 . sysPr0 $ s, sysPr1 = clearQueueS . sysPr1 $ s}
  | flag1 == Wait && (not . null) qs0 = runSystem $ s {sysPr0 = clearQueueS . sysPr0 $ s, sysPr1 = setQueueR qs0 . sysPr1 $ s}
  | flag0 == Wait && flag1 == Wait = s -- deadlock
  | flag0 == Running = runSystem $ s {sysPr0 = runProgramUntil (const False) . sysPr0 $ s}
  | flag1 == Running = runSystem $ s {sysPr1 = runProgramUntil (const False) . sysPr1 $ s}
  where
    qs0 = reverse . prToSend . sysPr0 $ s
    qs1 = reverse . prToSend . sysPr1 $ s
    flag0 = (prFlag . sysPr0) s
    flag1 = (prFlag . sysPr1) s
    debugInfo pr = (prPos pr, regVal (Reg "p") pr, prToRecv pr, prToSend pr, prFlag pr)

fixOp :: Op -> Op
fixOp (Snd arg) = Send arg
fixOp (Rcv reg) = Recv reg
fixOp op = op

fixProgram :: Program -> Program
fixProgram pr = pr {prOps = map fixOp . prOps $ pr}

part1Solution :: [Op] -> Int
part1Solution = prSnd . runProgramUntil isRecoverExecuted . mkProgram

part2Solution :: [Op] -> Int
part2Solution = prSentCnt . sysPr1 . runSystem . mkSystem
