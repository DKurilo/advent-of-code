module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (first)
import Data.Bits
import Data.Char (isAlpha, isLower)
import qualified Data.Map as M
import Data.Word (Word16 (..))
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Val = Val Word16 deriving (Show)

instance Read Val where
  readPrec = Val <$> readPrec

newtype Var = Var String deriving (Eq, Ord, Show)

instance Read Var where
  readPrec = do
    Var <$> (lift . P.munch1) (\c -> isAlpha c && isLower c)

data Arg = AVal Val | AVar Var deriving (Show)

instance Read Arg where
  readPrec = do
    (AVal <$> readPrec) PC.+++ (AVar <$> readPrec)

data BinOp = And | Or | LShift | RShift deriving (Show)

data Op = Let Arg | Not Arg | Bin BinOp Arg Arg deriving (Show)

readBin :: String -> (Arg -> Arg -> Op) -> ReadPrec Op
readBin cs op = do
  arg1 <- readPrec
  (lift . P.char) ' '
  (lift . P.string) cs
  (lift . P.char) ' '
  op arg1 <$> readPrec

readAnd = readBin "AND" (Bin And)

readOr = readBin "OR" (Bin Or)

readLShift = readBin "LSHIFT" (Bin LShift)

readRShift = readBin "RSHIFT" (Bin RShift)

readLet = do
  Let <$> readPrec

readNot = do
  (lift . P.string) "NOT "
  Not <$> readPrec

instance Read Op where
  readPrec = readLet PC.+++ readNot PC.+++ readAnd PC.+++ readOr PC.+++ readLShift PC.+++ readRShift

data Stat = Stat Op Var deriving (Show)

instance Read Stat where
  readPrec = do
    op <- readPrec
    (lift . P.string) " -> "
    Stat op <$> readPrec

type Context = M.Map Var Op

mkContext :: [Stat] -> Context
mkContext = foldl (\ctx (Stat op var) -> M.insert var op ctx) M.empty

evalArg :: Arg -> Context -> (Maybe Word16, Context)
evalArg (AVal (Val x)) ctx = (Just x, ctx)
evalArg (AVar var) ctx = evalReg var ctx

evalOp :: Op -> Context -> (Maybe Word16, Context)
evalOp (Let arg) ctx = evalArg arg ctx
evalOp (Not arg) ctx = first (complement <$>) $ evalArg arg ctx
evalOp (Bin op arg1 arg2) ctx = ((f <$> v1) <*> v2, ctx'')
  where
    (v1, ctx') = evalArg arg1 ctx
    (v2, ctx'') = evalArg arg2 ctx'
    f = case op of
      And -> (.&.)
      Or -> (.|.)
      LShift -> (\x y -> shift x . fromIntegral $ y)
      RShift -> (\x y -> shift x . negate . fromIntegral $ y)

evalReg :: Var -> Context -> (Maybe Word16, Context)
evalReg var ctx = (v, ctx'')
  where
    (v, ctx') = case var `M.lookup` ctx of
      Just op -> evalOp op ctx
      _ -> (Nothing, ctx)
    ctx'' = case v of
      Just x -> M.insert var (Let (AVal (Val x))) ctx'
      _ -> ctx'

part1Solution :: [Stat] -> Maybe Word16
part1Solution = fst . evalReg (Var "a") . mkContext

part2Solution :: [Stat] -> Maybe Word16
part2Solution statements = case evalReg (Var "a") ctx of
  (Just x, _) -> fst $ evalReg (Var "a") (M.insert (Var "b") (Let (AVal (Val x))) ctx)
  _ -> Nothing
  where
    ctx = mkContext statements
