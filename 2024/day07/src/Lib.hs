module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Text.ParserCombinators.ReadP as P
import Text.Read
import Data.Char (isDigit)

data PEq = PEq {peTest :: Int, peNs :: [Int]} deriving (Eq, Show)

readNs :: P.ReadP [Int]
readNs = do
    n <- P.munch1 isDigit
    P.skipSpaces
    ns <- readNs P.<++ return []
    return (read n:ns)

instance Read PEq where
    readPrec = lift $ do
        x <- P.munch1 isDigit
        _ <- P.char ':'
        P.skipSpaces
        PEq (read x) <$> readNs

data Op = Add | Mul | Conc deriving (Show)

data Expr = ExprI Expr Op Expr | ExprL Int deriving (Show)

data Eqt = Eqt {eTest :: Int, eExpr :: Expr}

eval :: Expr -> Int
eval (ExprI expr1 op expr2) = case op of
                                  Add -> eval expr1 + eval expr2
                                  Mul -> eval expr1 * eval expr2
                                  Conc -> read ((show . eval) expr1 <> (show . eval) expr2)
eval (ExprL x) = x

isGood :: Eqt -> Bool
isGood eq = eTest eq == (eval . eExpr) eq

possibleExpr :: [Op] -> [Int] -> [Expr]
possibleExpr ops = possibleExpr' ops . reverse

possibleExpr' :: [Op] -> [Int] -> [Expr]
possibleExpr' _ [] = []
possibleExpr' _ [x] = [ExprL x]
possibleExpr' ops (x:xs) = concatMap (\op -> fmap (\ex -> ExprI ex op (ExprL x)) exs) ops
    where
      exs = possibleExpr' ops xs

solution :: [Op] -> [String] -> Int
solution ops = sum
             . fmap peTest
             . filter (\(PEq x ns) -> (any (isGood . Eqt x)
             . possibleExpr ops) ns)
             . fmap read

part1Solution :: [String] -> Int
part1Solution = solution [Add, Mul]

part2Solution :: [String] -> Int
part2Solution = solution [Add, Mul, Conc]
