module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import qualified Data.Set                     as S
import qualified Data.Vector                  as V
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read                    hiding (step)

data Op = NOP | ACC | JMP deriving (Show, Eq)

instance Read Op where
    readPrec = lift $
        (do
            P.string "nop"
            return NOP) <|>
        (do
            P.string "acc"
            return ACC) <|>
        (do
            P.string "jmp"
            return JMP)

data Instruction = Instruction Op Int deriving (Show, Eq)

instance Read Instruction where
    readPrec = do
        op <- readPrec
        lift P.skipSpaces
        sign <- lift $ P.char '-' <|> P.char '+'
        lift P.skipSpaces
        n <- readPrec
        return $ Instruction op (if sign == '-' then (-n) else n)

data Program = Program Int Int (V.Vector Instruction) deriving (Show)

mkProgram :: [Instruction] -> Program
mkProgram = Program 0 0 . V.fromList

type Log = S.Set Int

data ErrCode = SUCCESS | FAIL | LOOP | EXEC deriving (Eq, Show)

data State = St ErrCode Log Program deriving (Show)

step :: State -> State
step (St EXEC log (Program acc line is)) = case is V.!? line of
                                             Just (Instruction NOP _) -> St EXEC log' (Program acc (line + 1) is)
                                             Just (Instruction ACC n) -> St EXEC log' (Program (acc + n) (line + 1) is)
                                             Just (Instruction JMP n) -> St EXEC log' (Program acc (line + n) is)
                                             Nothing -> error "wrong line"
    where log' = line `S.insert` log
step s = s

execute :: State -> State
execute s@(St EXEC log (Program acc line is))
  | isLoop s s' = St LOOP log (Program acc line is)
  | isSuccess s s' = St SUCCESS log' (Program acc' line' is)
  | isFailed s s' = St FAIL log (Program acc line is)
  | otherwise = execute s'
    where s'@(St _ log' (Program acc' line' _)) = step s
execute s = s

isLoop :: State -> State -> Bool
isLoop _ (St _ log (Program _ line _)) = line `S.member` log

isSuccess:: State -> State -> Bool
isSuccess (St _ _ (Program _ line is)) (St _ _ (Program _ line' _))= line == (V.length is - 1) && line' == V.length is

isFailed :: State -> State -> Bool
isFailed (St _ _ (Program _ _ is)) (St _ _ (Program _ line' _))= line' >= V.length is || line' < 0

getAcc :: State -> Int
getAcc (St _ _ (Program acc _ _)) = acc

fixOp :: State -> Int -> State
fixOp st@(St code s (Program acc line is)) n = case is V.!? n of
  Just (Instruction NOP v) -> St code s (Program acc line (is V.// [(n, Instruction JMP v)]))
  Just (Instruction JMP v) -> St code s (Program acc line (is V.// [(n, Instruction NOP v)]))
  _ -> st

input :: IO State
input = St EXEC S.empty . mkProgram . map read . lines <$> readFile "input"

part1solution :: IO ()
part1solution = print . getAcc . execute =<< input

part2solution :: IO ()
part2solution = do
    s@(St _ _ (Program _ _ is)) <- input
    print . map getAcc . filter (\(St code _ _) -> code == SUCCESS) . map (execute . fixOp s . fst)
          . filter (\(_, Instruction op _) -> op == NOP || op == JMP) . zip [0..] . V.toList $ is
