module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (guard)
import Data.Bifunctor (bimap, second)
import Data.Char (isAlpha)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Direction = U | D | R | L | Final deriving (Eq, Show)

startingPoint :: [String] -> (Int, Int, Direction)
startingPoint = head . filter (\(_, _, d) -> d == D) . zipWith (\i c -> if c == '|' then (i, 0, D) else (i, 0, Final)) [0 ..] . head

findRoute :: [String] -> (Int, Int, Direction) -> (String, Int)
findRoute route (x, y, d)
  | null nexts = ([c], 1)
  | isAlpha c = bimap (c :) (+ 1) rest
  | otherwise = second (+ 1) rest
  where
    c = route !! y !! x
    h = length route
    nextsBothDirections = do
      (dx, dy, d') <- [(0, 1, D), (0, -1, U), (1, 0, R), (-1, 0, L)]
      guard $ case d of
        U -> d' /= D
        D -> d' /= U
        R -> d' /= L
        L -> d' /= R
      let y' = y + dy
      guard $ y' >= 0 && y' <= h
      let x' = x + dx
      guard $ x' >= 0 && x' < length (route !! y')
      let c' = route !! y' !! x'
      guard $ c' /= ' '
      return (x', y', d')
    nextsSameDirection = filter (\(_, _, d') -> d' == d) nextsBothDirections
    nexts = if null nextsSameDirection then nextsBothDirections else nextsSameDirection
    rest = findRoute route . head $ nexts

part1Solution :: [String] -> String
part1Solution route = fst . findRoute route . startingPoint $ route

part2Solution :: [String] -> Int
part2Solution route = snd . findRoute route . startingPoint $ route
