module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

data Expr = Numb Int | Mul Expr Expr | Add Expr Expr | Par Expr deriving (Show)

pNumb :: ReadPrec Expr
pNumb = do
    lift P.skipSpaces
    n <- readPrec
    lift P.skipSpaces
    return (Numb n)

pPar :: ReadPrec Expr
pPar = do
    lift P.skipSpaces
    lift $ P.char '('
    e <- readPrec
    lift $ P.char ')'
    lift P.skipSpaces
    return (Par e)

pOp :: ReadPrec Expr
pOp = do
    e <- pPar <|> pNumb
    lift P.skipSpaces
    op <- lift (P.char '+' <|> P.char '*')
    lift P.skipSpaces
    case op of
      '+' -> Add e <$> readPrec
      '*' -> Mul e <$> readPrec
      _   -> pfail

instance Read Expr where
    readPrec = pOp <|> pPar <|> pNumb

input :: IO [Expr]
input =  map read . lines <$> readFile "input"

eval1 :: (Int -> Int) -> Expr -> Int
eval1 f (Numb x)    = f x
eval1 f (Mul e1 e2) = eval1 (*f (eval1 id e1)) e2
eval1 f (Add e1 e2) = eval1 (+f (eval1 id e1)) e2
eval1 f (Par e)     = f (eval1 id e)

eval2 :: (Int -> Int) -> Expr -> Int
eval2 f (Numb x)    = f x
eval2 f (Mul e1 e2) = eval2 f e1 * eval2 id e2
eval2 f (Add e1 e2) = eval2 (+f (eval2 id e1)) e2
eval2 f (Par e)     = f (eval2 id e)

part1solution :: IO ()
part1solution = print . sum . map (eval1 id) =<< input

part2solution :: IO ()
part2solution = print . sum . map (eval2 id) =<< input
