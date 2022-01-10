module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read (Read (..), lift)

data Register = A | B deriving (Show)

instance Read Register where
  readPrec = lift $ (P.char 'a' >> return A) P.+++ (P.char 'b' >> return B)

type Offset = Int

data Op = HLF Register | TPL Register | INC Register | JMP Offset | JIE Register Offset | JIO Register Offset deriving (Show)

instance Read Op where
  readPrec =
    lift $
      ( do
          P.string "hlf"
          P.char ' '
          HLF <$> PC.readPrec_to_P readPrec 0
      )
        P.+++ ( do
                  P.string "tpl"
                  P.char ' '
                  TPL <$> PC.readPrec_to_P readPrec 0
              )
        P.+++ ( do
                  P.string "inc"
                  P.char ' '
                  INC <$> PC.readPrec_to_P readPrec 0
              )
        P.+++ ( do
                  P.string "jmp"
                  P.char ' '
                  P.many . P.char $ '+'
                  JMP <$> PC.readPrec_to_P readPrec 0
              )
        P.+++ ( do
                  P.string "jie"
                  P.char ' '
                  r <- PC.readPrec_to_P readPrec 0
                  P.string ", "
                  P.many . P.char $ '+'
                  JIE r <$> PC.readPrec_to_P readPrec 0
              )
        P.+++ ( do
                  P.string "jio"
                  P.char ' '
                  r <- PC.readPrec_to_P readPrec 0
                  P.string ", "
                  P.many . P.char $ '+'
                  JIO r <$> PC.readPrec_to_P readPrec 0
              )

type Position = Int

type RA = Int

type RB = Int

data Program = Program [Op] Position RA RB

step :: Program -> Program
step p@(Program ops pos ra rb)
  | pos < 0 || pos >= length ops = p
  | otherwise = applyOp (ops !! pos)
  where
    applyOp (HLF A) = Program ops (pos + 1) (ra `div` 2) rb
    applyOp (HLF B) = Program ops (pos + 1) ra (rb `div` 2)
    applyOp (TPL A) = Program ops (pos + 1) (ra * 3) rb
    applyOp (TPL B) = Program ops (pos + 1) ra (rb * 3)
    applyOp (INC A) = Program ops (pos + 1) (ra + 1) rb
    applyOp (INC B) = Program ops (pos + 1) ra (rb + 1)
    applyOp (JMP offset) = Program ops (pos + offset) ra rb
    applyOp (JIE A offset) = Program ops (if even ra then pos + offset else pos + 1) ra rb
    applyOp (JIE B offset) = Program ops (if even rb then pos + offset else pos + 1) ra rb
    applyOp (JIO A offset) = Program ops (if ra == 1 then pos + offset else pos + 1) ra rb
    applyOp (JIO B offset) = Program ops (if rb == 1 then pos + offset else pos + 1) ra rb

execute :: Program -> (RA, RB)
execute = (\(Program _ _ ra rb) -> (ra, rb)) . until (\(Program ops pos _ _) -> pos < 0 || pos >= length ops) step

part1Solution :: [Op] -> (RA, RB)
part1Solution ops = execute (Program ops 0 0 0)

part2Solution :: [Op] -> (RA, RB)
part2Solution ops = execute (Program ops 0 1 0)
