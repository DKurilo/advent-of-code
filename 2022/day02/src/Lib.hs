module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Hand = Rock | Paper | Scissors deriving (Eq, Show, Ord)

parseHand1 :: Char -> Hand
parseHand1 'A' = Rock
parseHand1 'B' = Paper
parseHand1 'C' = Scissors
parseHand1 _ = Rock

parseHand2 :: Char -> Hand
parseHand2 'X' = Rock
parseHand2 'Y' = Paper
parseHand2 'Z' = Scissors
parseHand2 _ = Rock

data Turn = Turn Hand Hand

data Result = Win | Lose | Draw

parseResult :: Char -> Result
parseResult 'X' = Lose
parseResult 'Y' = Draw
parseResult 'Z' = Win
parseResult _ = Lose

getResult :: Turn -> Result
getResult (Turn Scissors Rock) = Win
getResult (Turn Rock Scissors) = Lose
getResult (Turn h1 h2)
  | h1 == h2 = Draw
  | h2 > h1 = Win
  | otherwise = Lose

data Turn2 = Turn2 Hand Result

getHand :: Turn2 -> Hand
getHand (Turn2 h Draw) = h
getHand (Turn2 Rock Win) = Paper
getHand (Turn2 Rock Lose) = Scissors
getHand (Turn2 Paper Win) = Scissors
getHand (Turn2 Paper Lose) = Rock
getHand (Turn2 Scissors Win) = Rock
getHand (Turn2 Scissors Lose) = Paper

parseStartegy :: String -> [Turn]
parseStartegy = map parseTurn . lines
  where
    parseTurn [h1, _, h2] = Turn (parseHand1 h1) (parseHand2 h2)
    parseTurn _ = Turn Rock Rock

parseStartegy2 :: String -> [Turn2]
parseStartegy2 = map parseTurn . lines
  where
    parseTurn [h1, _, h2] = Turn2 (parseHand1 h1) (parseResult h2)
    parseTurn _ = Turn2 Rock Lose

handScore :: Hand -> Int
handScore Rock = 1
handScore Paper = 2
handScore Scissors = 3

resultScore :: Result -> Int
resultScore Win = 6
resultScore Draw = 3
resultScore Lose = 0

score :: Turn -> Int
score t@(Turn _ h2) = resultScore result + handScore h2
  where
    result = getResult t

score2 :: Turn2 -> Int
score2 t@(Turn2 _ result) = resultScore result + handScore h2
  where
    h2 = getHand t

part1Solution :: String -> Int
part1Solution = sum . map score . parseStartegy

part2Solution :: String -> Int
part2Solution = sum . map score2 . parseStartegy2
