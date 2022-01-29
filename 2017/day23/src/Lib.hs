module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Data.List (dropWhile, takeWhile)
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
  = Set Register Argument
  | Sub Register Argument
  | Mul Register Argument
  | Jnz Argument Argument
  deriving (Show)

instance Read Op where
  readPrec =
    ( do
        lift . P.string $ "set"
        lift P.skipSpaces
        r <- readPrec
        lift P.skipSpaces
        Set r <$> readPrec
    )
      PC.<++ ( do
                 lift . P.string $ "sub"
                 lift P.skipSpaces
                 r <- readPrec
                 lift P.skipSpaces
                 Sub r <$> readPrec
             )
      PC.<++ ( do
                 lift . P.string $ "mul"
                 lift P.skipSpaces
                 r <- readPrec
                 lift P.skipSpaces
                 Mul r <$> readPrec
             )
      PC.<++ ( do
                 lift . P.string $ "jnz"
                 lift P.skipSpaces
                 r <- readPrec
                 lift P.skipSpaces
                 Jnz r <$> readPrec
             )

data Program = Program
  { prOps :: [Op],
    prPos :: Int,
    prCtx :: M.Map Register Int,
    prMuls :: Int
  }
  deriving (Show)

mkProgram :: [Op] -> Program
mkProgram ops = Program ops 0 M.empty 0

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
applyOp (Set reg arg) pr = applyBin reg (\_ x -> x) arg pr
applyOp (Sub reg arg) pr = applyBin reg (-) arg pr
applyOp (Mul reg arg) pr = applyBin reg (*) arg $ pr {prMuls = prMuls pr + 1}
applyOp (Jnz x y) pr
  | argVal x pr /= 0 = jump (argVal y pr) pr
  | otherwise = next pr

runProgramUntil :: (Program -> Bool) -> Program -> Program
runProgramUntil f pr
  | pos < 0 || pos >= length ops = pr
  | f pr = pr
  | otherwise = runProgramUntil f . applyOp (ops !! pos) $ pr
  where
    pos = prPos pr
    ops = prOps pr

part1Solution :: [Op] -> Int
part1Solution = prMuls . runProgramUntil (const False) . mkProgram

primes :: [Int]
primes = 2 : filter isPrime [3 ..]

isPrime :: Int -> Bool
isPrime n = all (\x -> n `mod` x /= 0) . takeWhile (<= (n `div` 2)) $ primes

part2Solution :: [Op] -> Int
part2Solution _ = length . filter (not . isPrime) . takeWhile (<= 122700) $ [105700, 105717 ..]
