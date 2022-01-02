module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import qualified Data.Set                     as S
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

pReadDeck :: ReadPrec [Int]
pReadDeck = do
    c <- readPrec
    cs <- (do
        lift P.skipSpaces
        pReadDeck) <|> return []
    return (c:cs)

pSkipPlayer :: ReadPrec ()
pSkipPlayer = lift $ do
    P.string "Player"
    P.munch1 (/='\n')
    P.skipSpaces

data State = St [Int] [Int] deriving (Eq, Show, Ord)

instance Read State where
    readPrec = do
        pSkipPlayer
        d <- pReadDeck
        lift P.skipSpaces
        pSkipPlayer
        St d <$> pReadDeck

input :: IO State
input = read <$> readFile "input"

playRound1 :: State -> State
playRound1 (St [] d2) = St [] d2
playRound1 (St d1 []) = St d1 []
playRound1 (St (c1:d1) (c2:d2))
  | c1 >= c2 = St (d1 ++ [c1, c2]) d2
  | otherwise = St d1 (d2 ++ [c2, c1])

play1 :: State -> [Int]
play1 (St [] d2) = d2
play1 (St d1 []) = d1
play1 st         = play1 . playRound1 $ st

score :: [Int] -> Int
score = sum . zipWith (*) [1..] . reverse

part1solution :: IO ()
part1solution = print . score . play1 =<< input

playRound2 :: S.Set State -> State -> (S.Set State, State)
playRound2 sts (St [] d2) = (sts, St [] d2)
playRound2 sts (St d1 []) = (sts, St d1 [])
playRound2 sts (St (c1:d1) (c2:d2))
  | length d1 >= c1 && length d2 >= c2 = (sts, if isPlayer1Won st' then stp1 else stp2)
  | c1 >= c2 = (sts, stp1)
  | otherwise = (sts, stp2)
    where (_, st') = play2 sts (St (take c1 d1) (take c2 d2))
          stp1 = St (d1 ++ [c1, c2]) d2
          stp2 = St d1 (d2 ++ [c2, c1])

play2 :: S.Set State -> State -> (S.Set State, State)
play2 sts st@(St d1 _) | st `S.member` sts = (sts, st)
play2 sts (St [] d2) = (sts, St [] d2)
play2 sts (St d1 []) = (sts, St d1 [])
play2 sts st         = play2 (S.insert st sts') st'
    where (sts', st') = playRound2 sts st

isPlayer1Won :: State -> Bool
isPlayer1Won (St [] _) = False
isPlayer1Won (St _ []) = True
isPlayer1Won _         = True

getWinnerDeck :: State -> [Int]
getWinnerDeck (St [] d2) = d2
getWinnerDeck (St d1 []) = d1
getWinnerDeck (St d1 _)  = d1

part2solution :: IO ()
part2solution = print . score . getWinnerDeck . snd . play2 S.empty =<< input
