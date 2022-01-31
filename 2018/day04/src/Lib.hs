{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', maximumBy, nub, sort)
import qualified Data.Map as M
import Data.Ord (Ordering (..), compare)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Action = Begin Int | Sleep | Up deriving (Eq, Ord, Show)

instance Read Action where
  readPrec =
    ( do
        lift . P.string $ "Guard #"
        guard <- readPrec
        lift . P.string $ " begins shift"
        return $ Begin guard
    )
      PC.<++ ((lift . P.string) "falls asleep" >> return Sleep)
      PC.<++ ((lift . P.string) "wakes up" >> return Up)

data Record = Record {rYear :: Int, rMonth :: Int, rDay :: Int, rHour :: Int, rMinutes :: Int, rAction :: Action} deriving (Eq, Ord, Show)

instance Read Record where
  readPrec = do
    lift . P.char $ '['
    year <- readPrec
    lift . P.char $ '-'
    month <- readPrec
    lift . P.char $ '-'
    day <- readPrec
    lift . P.char $ ' '
    hours <- readPrec
    lift . P.char $ ':'
    minutes <- readPrec
    lift . P.char $ ']'
    lift P.skipSpaces
    action <- readPrec
    lift P.skipSpaces
    return $ Record year month day hours minutes action

data Shift = Shift {sGuard :: Int, sDreams :: S.Set Int} deriving (Show)

parseLog :: [Record] -> [Shift]
parseLog = (\(shs, Just sh, _) -> sh : shs) . foldl' doer ([], Nothing, Nothing) . sort
  where
    doer :: ([Shift], Maybe Shift, Maybe Int) -> Record -> ([Shift], Maybe Shift, Maybe Int)
    doer (shs, Nothing, _) (Record _ _ _ _ _ (Begin guard)) = (shs, Just (Shift guard S.empty), Nothing)
    doer (shs, Just sh, _) (Record _ _ _ _ _ (Begin guard)) = (sh : shs, Just (Shift guard S.empty), Nothing)
    doer (shs, Just sh, _) (Record _ _ _ _ minutes Sleep) = (shs, Just sh, Just minutes)
    doer (shs, Just sh, Just sleep) (Record _ _ _ _ minutes Up) =
      (shs, Just (sh {sDreams = sDreams sh `S.union` S.fromList [sleep .. minutes - 1]}), Nothing)
    doer x _ = x

onSnd :: Ord b => (a, b) -> (a, b) -> Ordering
onSnd (_, x1) (_, x2) = x1 `compare` x2

guardsSleeps :: [Shift] -> [(Int, Int)]
guardsSleeps =
  M.toList
    . foldl' (\sls sh -> M.insertWith (+) (sGuard sh) ((S.size . sDreams) sh) sls) M.empty

mostSleepy :: [Shift] -> Int
mostSleepy =
  fst
    . maximumBy onSnd
    . guardsSleeps

mostSleepyMinuteByGuard :: Int -> [Shift] -> (Int, Int)
mostSleepyMinuteByGuard guard shifts
  | null dreams = (0, 0)
  | otherwise = maximumBy onSnd . M.toList $ dreams
  where
    dreams = M.unionsWith (+) . map (M.fromList . map (,1) . S.toList . sDreams) . filter ((== guard) . sGuard) $ shifts

part1Solution :: [Record] -> Int
part1Solution rs = guard * (fst . mostSleepyMinuteByGuard guard) shifts
  where
    shifts = parseLog rs
    guard = mostSleepy shifts

part2Solution :: [Record] -> Int
part2Solution rs = guard * minute
  where
    shifts = parseLog rs
    guards = nub . map sGuard $ shifts
    guardsSleepingHabbits = map (\guard -> (guard, mostSleepyMinuteByGuard guard shifts)) guards
    (guard, (minute, _)) = maximumBy (\(_, (_, x1)) (_, (_, x2)) -> x1 `compare` x2) guardsSleepingHabbits
