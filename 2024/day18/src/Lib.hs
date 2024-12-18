module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = P
  { px :: Int
  , py :: Int
  } deriving (Eq, Ord, Show)

instance Read Point where
  readPrec = do
    x <- readPrec
    _ <- lift . P.char $ ','
    P x <$> readPrec

data FB = FB
  { fbBs :: [Point]
  , fbW :: Int
  , fbH :: Int
  , fbP1 :: Int
  } deriving (Show)

instance Read FB where
  readPrec =
    lift $ do
      _ <- P.char '#'
      w <- PC.readPrec_to_P readPrec 0
      _ <- P.char ','
      h <- PC.readPrec_to_P readPrec 0
      _ <- P.char ','
      p1 <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      bs <-
        P.many1 $ do
          p <- PC.readPrec_to_P readPrec 0
          P.skipSpaces
          return p
      return $ FB bs w h p1

findPathLength :: FB -> Maybe Int -> Maybe Int
findPathLength fb mbn = doer 0 (S.singleton (P 0 0)) (S.singleton (P 0 0))
  where
    w = fbW fb
    h = fbH fb
    n =
      case mbn of
        Just n' -> n'
        Nothing -> fbP1 fb
    bs = S.fromList . take n . fbBs $ fb
    doer :: Int -> S.Set Point -> S.Set Point -> Maybe Int
    doer st front visited
      | S.null front = Nothing
      | P (w - 1) (h - 1) `S.member` front = Just st
      | otherwise = doer (st + 1) front' (visited `S.union` front')
      where
        front' =
          S.fromList
            . concatMap
                (\p ->
                   [ p'
                   | (dx, dy) <- [(0, -1), (1, 0), (0, 1), (-1, 0)]
                   , let p' = P (px p + dx) (py p + dy)
                   , px p' >= 0
                       && px p' < w
                       && py p' >= 0
                       && py p' < h
                       && p' `S.notMember` bs
                       && p' `S.notMember` visited
                   ])
            . S.toList
            $ front

part1Solution :: String -> Int
part1Solution = fromMaybe (-1) . (`findPathLength` Nothing) . read

search :: FB -> Point
search fb = fbBs fb !! (doer m0 n0 - 1)
  where
    m0 = 1
    n0 = length . fbBs $ fb
    doer :: Int -> Int -> Int
    doer m n
      | m == n =
        case findPathLength fb (Just m) of
          Just _ -> m + 1
          Nothing -> m
      | n - m == 1 = n
      | otherwise =
        case findPathLength fb (Just k) of
          Just _ -> doer k n
          Nothing -> doer m k
      where
        k = (m + n) `div` 2

part2Solution :: String -> String
part2Solution = (\p -> (show . px) p <> "," <> (show . py) p) . search . read
