module Lib
  ( part1Solution,
    part2Solution,
    Op (..),
    Program (..),
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Register = W | X | Y | Z deriving (Eq, Show)

instance Read Register where
  readPrec =
    ((lift . P.char) 'w' >> return W)
      PC.+++ ((lift . P.char) 'x' >> return X)
      PC.+++ ((lift . P.char) 'y' >> return Y)
      PC.+++ ((lift . P.char) 'z' >> return Z)

data Arg = Reg Register | Val Int deriving (Eq, Show)

instance Read Arg where
  readPrec =
    do
      Reg <$> readPrec
      PC.+++ (Val <$> readPrec)

data OpType = Add | Mul | Div | Mod | Eql deriving (Eq, Show)

instance Read OpType where
  readPrec =
    ((lift . P.string) "add" >> return Add)
      PC.+++ ((lift . P.string) "mul" >> return Mul)
      PC.+++ ((lift . P.string) "div" >> return Div)
      PC.+++ ((lift . P.string) "mod" >> return Mod)
      PC.+++ ((lift . P.string) "eql" >> return Eql)

data Op = Inp Register | Binary OpType Register Arg | Comment deriving (Eq, Show)

instance Read Op where
  readPrec = do
    ( do
        (lift . P.string) "inp "
        Inp <$> readPrec
      )
      PC.+++ ( do
                 opType <- readPrec
                 (lift . P.char) ' '
                 reg <- readPrec
                 (lift . P.char) ' '
                 Binary opType reg <$> readPrec
             )
      PC.+++ ( do
                 (lift . P.char) '#'
                 (lift . P.many) P.get
                 return Comment
             )

type Program = [Op]

digits :: Int -> [Int]
digits = reverse . doer
  where
    doer n
      | n == 0 = []
      | otherwise = n `mod` 10 : doer (n `div` 10)

eval :: Program -> [Int] -> Int
eval program ns = doer ns program 0 0 0 0
  where
    doer :: [Int] -> Program -> Int -> Int -> Int -> Int -> Int
    doer _ [] _ _ _ z = z
    doer (n : ns) (Inp r : ops) w x y z
      | r == W = doer ns ops n x y z
      | r == X = doer ns ops w n y z
      | r == Y = doer ns ops w x n z
      | r == Z = doer ns ops w x y n
    doer ns (Binary opType r arg : ops) w x y z = doer ns ops w' x' y' z'
      where
        op = case opType of
          Add -> (+)
          Mul -> (*)
          Div -> div
          Mod -> mod
          eql -> (\x y -> if x == y then 1 else 0)
        arg2 = case arg of
          Reg W -> w
          Reg X -> x
          Reg Y -> y
          Reg Z -> z
          Val n -> n
        w' = if r == W then w `op` arg2 else w
        x' = if r == X then x `op` arg2 else x
        y' = if r == Y then y `op` arg2 else y
        z' = if r == Z then z `op` arg2 else z
    doer ns (Comment : ops) w x y z = doer ns ops w x y z

-- Actual solution is in day24.png
-- All you see here doesn't matter, actually. :)
part1Solution :: Program -> (Int, Int)
part1Solution p = (\x -> (eval p . digits $ x, x)) 97919997299495

part2Solution :: Program -> (Int, Int)
part2Solution p = (\x -> (eval p . digits $ x, x)) 51619131181131
