module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha, isDigit)
import Data.List (elemIndex, sort, sortBy)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

type Name = String

type Sector = Int

type Hash = String

data Room = Room Name Sector Hash

instance Read Room where
  readPrec = do
    name <- init <$> (lift . P.munch1) (not . isDigit)
    sector <- readPrec
    lift . P.char $ '['
    hash <- (lift . P.munch1) isAlpha
    lift . P.char $ ']'
    return $ Room name sector hash

getHash :: String -> String
getHash = take 5 . map snd . sortBy descFstAscSnd . fst . (`doer` '*') . foldl doer ([], (0, '*')) . sort . filter isAlpha
  where
    doer :: ([(Int, Char)], (Int, Char)) -> Char -> ([(Int, Char)], (Int, Char))
    doer (ls, l@(n, prev)) c
      | prev == '*' = ([], (1, c))
      | prev == c = (ls, (n + 1, c))
      | otherwise = (l : ls, (1, c))
    descFstAscSnd (n1, c1) (n2, c2)
      | n1 > n2 = LT
      | n1 < n2 = GT
      | otherwise = c1 `compare` c2

isReal :: Room -> Bool
isReal (Room cs _ hash) = hash == getHash cs

part1Solution :: [Room] -> Int
part1Solution = sum . map (\(Room _ s _) -> s) . filter isReal

abc = ['a' .. 'z']

caesar :: Int -> String -> String
caesar n = map doer
  where
    doer c = case c `elemIndex` abc of
      Just x -> abc !! ((x + n) `mod` length abc)
      _ -> c

part2Solution :: [Room] -> [(String, Sector)]
part2Solution = map (\(Room cs s _) -> (caesar (s `mod` length abc) cs, s)) . filter isReal
