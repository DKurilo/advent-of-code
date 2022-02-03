{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (replicate)
import qualified Data.Map as M
import qualified Data.Sequence as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Game = Game {gPs :: M.Map Int Int, gMs :: Int, gPCurr :: Int, gMCurr :: Int, gPos :: Int, gTable :: S.Seq Int} deriving (Show)

instance Read Game where
  readPrec = do
    ps <- readPrec
    lift . P.string $ " players; last marble is worth "
    ms <- readPrec
    lift . P.string $ " points"
    return $ Game (M.fromList . map (,0) $ [0 .. (ps - 1)]) ms 1 1 0 (S.fromList [0])

putMarbleOnTable :: Game -> Game
putMarbleOnTable g
  | gMCurr g `mod` 23 == 0 =
    g
      { gPs = (M.insertWith (+) (gPCurr g) (m23 + gMCurr g) . gPs) g,
        gPCurr = p,
        gMCurr = m,
        gPos = pos23,
        gTable = table23
      }
  | otherwise = g {gPCurr = p, gMCurr = m, gPos = pos, gTable = table}
  where
    pos23 = (gPos g - 7) `mod` (S.length . gTable) g
    m23 = gTable g `S.index` pos23
    table23 = (S.take pos23 . gTable) g S.>< (S.drop (pos23 + 1) . gTable) g
    p = (gPCurr g + 1) `mod` (M.size . gPs) g
    m = gMCurr g + 1
    pos = (gPos g + 1) `mod` (S.length . gTable) g + 1
    table = (S.take pos . gTable) g S.>< (gMCurr g S.<| (S.drop pos . gTable) g)

play :: Game -> Int
play g
  | gMCurr g == gMs g = maximum . M.elems . gPs $ g
  | otherwise = play . putMarbleOnTable $ g

part1Solution :: [Game] -> [Int]
part1Solution = map play

moreMarbles :: Game -> Game
moreMarbles g = g {gMs = gMs g * 100}

part2Solution :: [Game] -> [Int]
part2Solution = map (play . moreMarbles)
