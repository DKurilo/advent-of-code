{-# LANGUAGE TupleSections #-}

module Lib
  ( solution,
    parse,
  )
where

import Data.Bifunctor (second)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (trace)

type X = Int

type Y = Int

type Point = (X, Y)

data Status = Initial | Moved | Final deriving (Show, Eq)

type Energy = Int

data AType = A | B | C | D deriving (Show, Eq)

data Amphipod = Amphipod AType Status Energy deriving (Show)

type Depth = Int

type Cave = (M.Map Point Amphipod, Depth)

xForAType :: AType -> X
xForAType A = 3
xForAType B = 5
xForAType C = 7
xForAType D = 9

isFilled :: Cave -> AType -> Point -> Bool
isFilled cvd@(cv, d) t (x, y) = case (x, y) `M.lookup` cv of
  Just (Amphipod t' _ _) | t' == t -> True
  _ -> False

isFilledDown :: Cave -> AType -> Point -> Bool
isFilledDown cvd@(cv, d) t p@(x, y)
  | y == d = True
  | isFilled cvd t (x, y + 1) = isFilledDown cvd t (x, y + 1)
  | otherwise = False

isOnDesiredPlace :: Cave -> Point -> Amphipod -> Bool
isOnDesiredPlace cvd p@(x, y) a@(Amphipod t _ _) = xForAType t == x && isFilledDown cvd t p

desired :: Cave -> Amphipod -> [Point]
desired cvd@(cv, d) (Amphipod t _ _) = map fst . filter (not . snd) . map (\y -> ((x, y), isFilled cvd t (x, y))) . reverse $ [2 .. d]
  where
    x = xForAType t

energy :: Amphipod -> Int
energy (Amphipod _ _ e) = e

status :: Amphipod -> Status
status (Amphipod _ s _) = s

atype :: Amphipod -> AType
atype (Amphipod t _ _) = t

distance :: Cave -> Point -> Point -> Maybe Int
distance cvd@(cv, d) p1@(x1, y1) p2@(x2, y2)
  | p1 == p2 = Just 0
  | y1 > 1 && x1 /= x2 && stepUp `M.notMember` cv = (+ 1) <$> distance cvd stepUp p2
  | y1 == 1 && x1 /= x2 && stepX `M.notMember` cv = (+ 1) <$> distance cvd stepX p2
  | y1 < y2 && x1 == x2 && stepDown `M.notMember` cv = (+ 1) <$> distance cvd stepDown p2
  | otherwise = Nothing
  where
    stepUp = (x1, y1 - 1)
    stepDown = (x1, y1 + 1)
    stepX = (x1 + signum (x2 - x1), y1)

move :: Cave -> Point -> Point -> Amphipod -> Maybe Amphipod
move cvd@(cv, d) p1 p2 a = do
  dist <- distance cvd p1 p2
  let pds = desired cvd a
      s
        | (not . null) pds && head pds == p2 = Final
        | otherwise = case status a of
          Initial -> Moved
          Moved -> Final
      e = energy a + dist * stepScore a
  return $ Amphipod (atype a) s e

setStatus :: Status -> Amphipod -> Amphipod
setStatus s (Amphipod t _ e) = Amphipod t s e

isAmphipod :: Char -> Bool
isAmphipod 'A' = True
isAmphipod 'B' = True
isAmphipod 'C' = True
isAmphipod 'D' = True
isAmphipod _ = False

mkAmphipod :: Char -> Amphipod
mkAmphipod 'A' = Amphipod A Initial 0
mkAmphipod 'B' = Amphipod B Initial 0
mkAmphipod 'C' = Amphipod C Initial 0
mkAmphipod 'D' = Amphipod D Initial 0

stepScore :: Amphipod -> Int
stepScore (Amphipod A _ _) = 1
stepScore (Amphipod B _ _) = 10
stepScore (Amphipod C _ _) = 100
stepScore (Amphipod D _ _) = 1000

parse :: String -> Cave
parse cs =
  ( M.fromList
      . map (second mkAmphipod)
      . filter (isAmphipod . snd)
      . concat
      . zipWith (\y -> zipWith (\x c -> ((x, y), c)) [0 ..]) [0 ..]
      $ ls,
    length ls - 2
  )
  where
    ls = lines cs

possibleMoves :: Cave -> [Cave]
possibleMoves cvd@(cv, d)
  | null finalMove = concatMap findWays as
  | otherwise = [(movedToDesired, d)]
  where
    as = filter ((/= Final) . status . snd) . M.toList $ cv
    finalMove =
      fromMaybe []
        . sequence
        . filter isJust
        . map (\(p, a) -> let p' = head (desired cvd a) in ((p, p'),) <$> move cvd p p' a)
        $ as
    movedToDesired = (\((p1, p2), a) -> M.insert p2 a . M.delete p1 $ cv) . head $ finalMove
    findWays :: (Point, Amphipod) -> [Cave]
    findWays (p@(xA, yA), a) =
      maybe
        []
        (map (\(p', a') -> (M.insert p' a' . M.delete p $ cv, d)))
        . sequence
        . filter isJust
        . map (\p' -> (p',) <$> move cvd p p' a)
        $ ( case status a of
              Initial ->
                filter
                  ((\x -> x /= 3 && x /= 5 && x /= 7 && x /= 9) . fst)
                  [(x, 1) | x <- [1 .. 11]]
              _ -> []
          )

isFinal :: Cave -> Bool
isFinal = all ((== Final) . status) . M.elems . fst

findSolutions :: Cave -> [Cave]
findSolutions cvd
  | isFinal cvd = [cvd]
  | otherwise = (concatMap findSolutions . possibleMoves) cvd

fixCave :: Cave -> Cave
fixCave cvd@(cv, d) = (M.mapWithKey (\p a -> if isOnDesiredPlace cvd p a then setStatus Final a else a) cv, d)

solution :: Cave -> Int
solution = minimum . map (sum . map energy . M.elems . fst) . findSolutions . fixCave
