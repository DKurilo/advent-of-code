module Lib
  ( part1Solution
  , part2Solution
  ) where

import Control.Monad (join)
import Data.Bits (Bits(xor))
import Data.Char (isDigit)
import Data.List (find, intercalate)
import Data.Maybe (fromMaybe, isJust)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Comp = Comp
  { cProgram :: [Int]
  , cA :: Int
  , cB :: Int
  , cC :: Int
  , cPointer :: Int
  , cOut :: [Int]
  } deriving (Show)

readNumb :: P.ReadP Int
readNumb = read <$> P.munch1 isDigit

readNumbs :: P.ReadP [Int]
readNumbs =
  P.many $ do
    n <- readNumb
    P.skipSpaces
    _ <- P.optional . P.char $ ','
    P.skipSpaces
    return n

instance Read Comp where
  readPrec =
    lift $ do
      _ <- P.string "Register A:"
      P.skipSpaces
      a <- readNumb
      P.skipSpaces
      _ <- P.string "Register B:"
      P.skipSpaces
      b <- readNumb
      P.skipSpaces
      _ <- P.string "Register C:"
      P.skipSpaces
      c <- readNumb
      P.skipSpaces
      _ <- P.string "Program:"
      P.skipSpaces
      prg <- readNumbs
      return $ Comp prg a b c 0 []

readCombo :: Int -> Comp -> Int
readCombo n comp
  | n <= 3 = n
  | n == 4 = cA comp
  | n == 5 = cB comp
  | n == 6 = cC comp
  | otherwise = error "reserved"

runStep :: Comp -> Comp
runStep comp
  | command == 0 =
    comp
      { cA = cA comp `div` (2 ^ readCombo operand comp)
      , cPointer = cPointer comp + 2
      }
  | command == 1 =
    comp {cB = cB comp `xor` operand, cPointer = cPointer comp + 2}
  | command == 2 =
    comp {cB = readCombo operand comp `mod` 8, cPointer = cPointer comp + 2}
  | command == 3 =
    if cA comp == 0
      then comp {cPointer = cPointer comp + 2}
      else comp {cPointer = operand}
  | command == 4 =
    comp {cB = cB comp `xor` cC comp, cPointer = cPointer comp + 2}
  | command == 5 =
    comp
      { cOut = readCombo operand comp `mod` 8 : cOut comp
      , cPointer = cPointer comp + 2
      }
  | command == 6 =
    comp
      { cB = cA comp `div` (2 ^ readCombo operand comp)
      , cPointer = cPointer comp + 2
      }
  | otherwise =
    comp
      { cC = cA comp `div` (2 ^ readCombo operand comp)
      , cPointer = cPointer comp + 2
      }
  where
    commandAndOperand = take 2 . drop (cPointer comp) . cProgram $ comp
    command = head commandAndOperand
    operand = last commandAndOperand

eval :: Comp -> Comp
eval =
  head
    . dropWhile (\comp -> cPointer comp < (length . cProgram) comp)
    . iterate runStep

readOutput :: Comp -> String
readOutput = intercalate "," . fmap show . reverse . cOut

part1Solution :: String -> String
part1Solution = readOutput . eval . read

part2Solution :: String -> Int
part2Solution cs = foldl (\x y -> x * 8 + y) 0 . fromMaybe [] . doer $ []
  where
    comp = read cs
    revProg = reverse . cProgram $ comp
    lp = length . cProgram $ comp
    doer :: [Int] -> Maybe [Int]
    doer ns
      | length ns == lp = Just ns
      | otherwise =
        join
          . find isJust
          . fmap (\(_, n) -> doer (ns <> [n]))
          . filter (\(comp', _) -> revProg !! lns == cOut comp' !! lns)
          . fmap
              (\n ->
                 let ns' = take lp $ ns <> [n] <> repeat 0
                     a = foldl (\x y -> x * 8 + y) 0 ns'
                     comp' = comp {cA = a}
                  in (eval comp', n))
          $ [0 .. 7]
      where
        lns = length ns
