module Lib
  ( part1Solution,
    part2Solution,
    Player (..),
    Game (..),
  )
where

import qualified Data.Map as M
import Debug.Trace (trace)

type Throws = Int

type Score = Int

type Place = Int

data Player = Player Score Place deriving (Show)

score (Player s _) = s

data Game = Game Bool Throws Player Player deriving (Show)

maxScore (Game _ _ (Player s1 _) (Player s2 _)) = max s1 s2

minScore (Game _ _ (Player s1 _) (Player s2 _)) = min s1 s2

move :: Int -> Player -> Player
move t (Player s p) = Player s' p'
  where
    moves = (t + 1) `mod` 100 + (t + 2) `mod` 100 + (t + 3) `mod` 100
    p' = (p + moves - 1) `mod` 10 + 1
    s' = s + p'

turn :: Game -> Game
turn (Game isSecond t p1 p2) =
  if isSecond
    then Game False t' p1 p2'
    else Game True t' p1' p2
  where
    t' = t + 3
    p1' = move t p1
    p2' = move t p2

part1Solution :: Game -> Int
part1Solution =
  (\g@(Game _ t _ _) -> minScore g * t)
    . until (\g -> maxScore g >= 1000) turn

data Game2 = Game2 Bool Score Score Place Place deriving (Show, Eq, Ord)

initGame2 :: Game -> Game2
initGame2 (Game _ _ (Player _ p1) (Player _ p2)) = Game2 False 0 0 p1 p2

us = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

turn2 :: Game2 -> [(Game2, Int)]
turn2 (Game2 t s1 s2 p1 p2) = do
  (pm, pn) <- us
  let p1' = (p1 + pm - 1) `mod` 10 + 1
      p2' = (p2 + pm - 1) `mod` 10 + 1
      s1' = s1 + p1'
      s2' = s2 + p2'
  return $ if t then (Game2 False s1 s2' p1 p2', pn) else (Game2 True s1' s2 p1' p2, pn)

universes :: Game2 -> (Int, Int)
universes (Game2 t s1 s2 p1 p2) =
  [ [ [ [[universesFrom (Game2 t' s1' s2' p1' p2') | t' <- [False, True]] | p1' <- [0 .. 100]] | p2' <- [0 .. 100]
      ]
      | s1' <- [0 .. 30]
    ]
    | s2' <- [0 .. 30]
  ]
    !! s2
    !! s1
    !! p2
    !! p1
    !! (if t then 1 else 0)
  where
    universesFrom :: Game2 -> (Int, Int)
    universesFrom g@(Game2 _ s1' s2' _ _)
      | s1' >= 21 = (1, 0)
      | s2' >= 21 = (0, 1)
      | otherwise =
        foldl
          (\(w1, w2) (g', n) -> let (w1', w2') = universes g' in (w1 + w1' * n, w2 + w2' * n))
          (0, 0)
          . turn2
          $ g

part2Solution :: Game -> Int
part2Solution = uncurry max . universes . initGame2
