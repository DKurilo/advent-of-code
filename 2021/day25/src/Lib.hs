module Lib
  ( part1Solution,
    parse,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (second)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Debug.Trace (trace)

data Cucumber = S | E deriving (Show, Eq)

type X = Int

type Y = Int

type Point = (X, Y)

data Trench = Trench Int Int (M.Map Point Cucumber) deriving (Show)

parse :: [String] -> Trench
parse ls =
  Trench w h
    . M.fromList
    . map (second (\c -> if c == 'v' then S else E))
    . filter ((/= '.') . snd)
    . concat
    . zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0 ..]) [0 ..]
    $ ls
  where
    h = length ls
    w = length . head $ ls

moveCuc :: Int -> Int -> Cucumber -> M.Map Point Cucumber -> (M.Map Point Cucumber, Bool) -> (Point, Cucumber) -> (M.Map Point Cucumber, Bool)
moveCuc w h turn cucs (cucs', moved) ((x, y), c)
  | turn /= c = (cucs''', moved)
  | moved' = (cucs'', True)
  | otherwise = (cucs''', moved)
  where
    x' = if turn == E then (x + 1) `mod` w else x
    y' = if turn == S then (y + 1) `mod` h else y
    moved' = (x', y') `M.notMember` cucs
    cucs'' = M.insert (x', y') c cucs'
    cucs''' = M.insert (x, y) c cucs'

moveOnce :: Trench -> Cucumber -> Maybe Trench
moveOnce (Trench w h cucs) turn
  | moved = Just (Trench w h cucs')
  | otherwise = Nothing
  where
    turn' = if turn == S then E else S
    (cucs', moved) = foldl (moveCuc w h turn cucs) (M.empty, False) . M.toList $ cucs

step :: Trench -> Maybe Trench
step t = do
  ( do
      t' <- moveOnce t E
      moveOnce t' S
    )
    <|> moveOnce t E
    <|> moveOnce t S

waitForCucumbers :: Trench -> Int
waitForCucumbers = doer 1
  where
    doer n t = case step t of
      Just t' -> doer (n + 1) t'
      _ -> n

part1Solution :: Trench -> Int
part1Solution = waitForCucumbers
