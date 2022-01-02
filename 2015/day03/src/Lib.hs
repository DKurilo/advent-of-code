module Lib
  ( part1Solution,
    part2Solution,
    isDirection,
  )
where

import Data.Bifunctor (first, second)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Direction = N | E | S | W deriving (Show)

type X = Int

type Y = Int

type Home = (X, Y)

startPos = (S.singleton start, start)
  where
    start = (0, 0)

instance Read Direction where
  readPrec =
    ((lift . P.char) '^' >> return N)
      PC.+++ ((lift . P.char) '>' >> return E)
      PC.+++ ((lift . P.char) 'v' >> return S)
      PC.+++ ((lift . P.char) '<' >> return W)

isDirection :: Char -> Bool
isDirection c
  | c == '^' = True
  | c == '>' = True
  | c == 'v' = True
  | c == '<' = True
  | otherwise = False

visited :: [Direction] -> S.Set Home
visited = fst . foldl visit startPos
  where
    visit :: (S.Set Home, Home) -> Direction -> (S.Set Home, Home)
    visit (s, h) d = (S.insert h' s, h')
      where
        h' = case d of
          N -> second (\n -> n - 1) h
          E -> first (+ 1) h
          S -> second (+ 1) h
          W -> first (\n -> n - 1) h

part1Solution :: [Direction] -> Int
part1Solution = S.size . visited

part2Solution :: [Direction] -> Int
part2Solution ds = S.size (vs `S.union` vr)
  where
    dsi = zip [0 ..] ds
    filterDir o = map snd . filter ((\n -> n `mod` 2 == o) . fst) $ dsi
    dss = filterDir 0
    dsr = filterDir 1
    vs = visited dss
    vr = visited dsr
