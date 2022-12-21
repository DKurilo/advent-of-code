module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import qualified Data.Map as M
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Statement
  = Add String String
  | Sub String String
  | Mul String String
  | Div String String
  | Val Int
  deriving (Show)

instance Read Statement where
  readPrec =
    lift
      ( ( do
            s1 <- P.munch1 isAlpha
            P.skipSpaces
            _ <- P.char '+'
            P.skipSpaces
            Add s1 <$> P.munch1 isAlpha
        )
          P.<++ ( do
                    s1 <- P.munch1 isAlpha
                    P.skipSpaces
                    _ <- P.char '-'
                    P.skipSpaces
                    Sub s1 <$> P.munch1 isAlpha
                )
          P.<++ ( do
                    s1 <- P.munch1 isAlpha
                    P.skipSpaces
                    _ <- P.char '*'
                    P.skipSpaces
                    Mul s1 <$> P.munch1 isAlpha
                )
          P.<++ ( do
                    s1 <- P.munch1 isAlpha
                    P.skipSpaces
                    _ <- P.char '/'
                    P.skipSpaces
                    Div s1 <$> P.munch1 isAlpha
                )
          P.<++ ( Val
                    <$> PC.readPrec_to_P readPrec 0
                )
      )

newtype Monkeys = Monkeys {unMonkeys :: M.Map String Statement} deriving (Show)

instance Read Monkeys where
  readPrec =
    Monkeys . M.fromList
      <$> lift
        ( P.many1
            ( do
                name <- P.munch1 isAlpha
                P.skipSpaces
                _ <- P.char ':'
                P.skipSpaces
                statement <- PC.readPrec_to_P readPrec 0
                P.skipSpaces
                return (name, statement)
            )
        )

eval :: String -> Monkeys -> (Int, Monkeys)
eval name ms = case name `M.lookup` unMonkeys ms of
  Just (Val x) -> (x, ms)
  Just (Add name1 name2) -> applyOp name1 name2 (+)
  Just (Sub name1 name2) -> applyOp name1 name2 (-)
  Just (Mul name1 name2) -> applyOp name1 name2 (*)
  Just (Div name1 name2) -> applyOp name1 name2 div
  _ -> error "monkey not found"
  where
    applyOp :: String -> String -> (Int -> Int -> Int) -> (Int, Monkeys)
    applyOp name1 name2 f = (x, Monkeys $ M.insert name (Val x) (unMonkeys ms''))
      where
        (x1, ms') = eval name1 ms
        (x2, ms'') = eval name2 ms'
        x = f x1 x2

getNames :: Statement -> (String, String)
getNames (Val _) = error "not operation"
getNames (Add name1 name2) = (name1, name2)
getNames (Sub name1 name2) = (name1, name2)
getNames (Mul name1 name2) = (name1, name2)
getNames (Div name1 name2) = (name1, name2)

part1Solution :: Monkeys -> Int
part1Solution = fst . eval "root"

monkeysToOps :: Monkeys -> M.Map String (Rational, Rational)
monkeysToOps (Monkeys ms) = snd . doer "root" $ M.empty
  where
    doer :: String -> M.Map String (Rational, Rational) -> ((Rational, Rational), M.Map String (Rational, Rational))
    doer "humn" ops = ((1, 0), M.insert "humn" (1, 0) ops)
    doer name ops = case (name `M.lookup` ops, name `M.lookup` ms) of
      (Just op, _) -> (op, ops)
      (_, Just (Val x)) -> ((0, fromIntegral x), M.insert name (0, fromIntegral x) ops)
      (_, Just (Add n1 n2)) -> applyOp n1 n2 (\(a1, b1) (a2, b2) -> (a1 + a2, b1 + b2))
      (_, Just (Sub n1 n2)) -> applyOp n1 n2 (\(a1, b1) (a2, b2) -> (a1 - a2, b1 - b2))
      (_, Just (Mul n1 n2)) ->
        applyOp
          n1
          n2
          ( \(a1, b1) (a2, b2) ->
              if a1 /= 0 && a2 /= 0
                then error "too complex, x^2"
                else (a1 * b2 + a2 * b1, b1 * b2)
          )
      (_, Just (Div n1 n2)) ->
        applyOp
          n1
          n2
          ( \(a1, b1) (a2, b2) ->
              if a2 /= 0
                then error "too complex, 1/x"
                else (a1 / b2, b1 / b2)
          )
      _ -> error "name not found"
      where
        applyOp ::
          String ->
          String ->
          ((Rational, Rational) -> (Rational, Rational) -> (Rational, Rational)) ->
          ((Rational, Rational), M.Map String (Rational, Rational))
        applyOp n1 n2 f = (op, M.insert name op ops'')
          where
            (op1, ops') = doer n1 ops
            (op2, ops'') = doer n2 ops'
            op = f op1 op2

part2Solution :: Monkeys -> Int
part2Solution ms = case (name1 `M.lookup` ops, name2 `M.lookup` ops) of
  (Just (a1, b1), Just (a2, b2)) -> ceiling . fromRational $ (b1 - b2) / (a2 - a1)
  _ -> error "can't solve"
  where
    ops = monkeysToOps ms
    (name1, name2) = case "root" `M.lookup` unMonkeys ms of
      Just s -> getNames s
      _ -> error "root not found"
