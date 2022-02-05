module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (second)
import Data.List (elemIndex)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Pots = Pots {unPots :: S.Set Int} deriving (Show, Eq)

isPot :: Char -> Bool
isPot '.' = True
isPot '#' = True
isPot _ = False

isPlant :: Char -> Bool
isPlant '.' = False
isPlant '#' = True
isPlant _ = False

instance Read Pots where
  readPrec = fmap (Pots . S.fromList . map fst . filter snd . zip [0 ..] . map isPlant) . lift . P.munch1 $ isPot

newtype Rules = Rules {unRules :: S.Set (Bool, Bool, Bool, Bool, Bool)} deriving (Show, Eq)

instance Read Rules where
  readPrec = fmap (Rules . S.unions) . lift . P.many1 $ do
    pots <- fmap ((\[p1, p2, p3, p4, p5] -> (p1, p2, p3, p4, p5)) . map isPlant) . P.count 5 . P.satisfy $ isPot
    P.string " => "
    p <- P.satisfy isPot
    P.skipSpaces
    return $ if isPlant p then S.singleton pots else S.empty

data Tunnel = Tunnel {tPots :: Pots, tRules :: Rules} deriving (Show, Eq)

instance Read Tunnel where
  readPrec = do
    lift . P.string $ "initial state:"
    lift P.skipSpaces
    pots <- readPrec
    lift P.skipSpaces
    Tunnel pots <$> readPrec

patterns :: Pots -> [(Int, (Bool, Bool, Bool, Bool, Bool))]
patterns pots = [(i, (potIn (i - 2), potIn (i - 1), potIn i, potIn (i + 1), potIn (i + 2))) | i <- [minI .. maxI]]
  where
    minI = (S.findMin . unPots) pots - 2
    maxI = (S.findMax . unPots) pots + 2
    potIn = (`S.member` unPots pots)

applyRules :: Rules -> [(Int, (Bool, Bool, Bool, Bool, Bool))] -> Pots
applyRules rules = Pots . S.fromList . map fst . filter snd . map (second nextPot)
  where
    nextPot = (`S.member` unRules rules)

day :: Tunnel -> Tunnel
day t = t {tPots = applyRules (tRules t) . patterns . tPots $ t}

tunnelValue :: Tunnel -> Int
tunnelValue = sum . S.toList . unPots . tPots

part1Solution :: Tunnel -> Int
part1Solution = tunnelValue . (!! 20) . iterate day

part2Solution :: Tunnel -> Int
part2Solution t = startValue + (50000000000 - startDay) * d
  where
    xs = map tunnelValue . iterate day $ t
    diffs = zipWith (\x1 x2 -> (x1, x2 - x1)) xs . drop 1 $ xs
    (startDay, ((startValue, d), _)) = head . filter (\(_, ((_, d1), (_, d2))) -> d1 == d2) . zip [0 ..] . zip diffs . drop 1 $ diffs
