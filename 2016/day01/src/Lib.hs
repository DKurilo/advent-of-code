module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Maybe (isJust)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read (Read (..), lift)

data Rotation = L | R | F deriving (Eq, Show)

instance Read Rotation where
  readPrec = lift $ (P.char 'L' >> return L) P.+++ (P.char 'R' >> return R)

data Step = Step Rotation Int deriving (Eq, Show)

instance Read Step where
  readPrec = do
    r <- readPrec
    Step r <$> readPrec

newtype Route = Route [Step] deriving (Eq, Show)

steps (Route ss) = ss

instance Read Route where
  readPrec = fmap Route . lift . P.many $ do
    s <- PC.readPrec_to_P readPrec 0
    P.many
      ( do
          P.string ", "
      )
    return s

data Direction = N | E | S | W deriving (Eq, Show)

type X = Int

type Y = Int

type Coord = (X, Y, Direction)

step :: Coord -> Step -> Coord
step (x, y, N) (Step R n) = (x + n, y, E)
step (x, y, E) (Step R n) = (x, y + n, S)
step (x, y, S) (Step R n) = (x - n, y, W)
step (x, y, W) (Step R n) = (x, y - n, N)
step (x, y, N) (Step L n) = (x - n, y, W)
step (x, y, E) (Step L n) = (x, y - n, N)
step (x, y, S) (Step L n) = (x + n, y, E)
step (x, y, W) (Step L n) = (x, y + n, S)
step (x, y, N) (Step F n) = (x, y - n, N)
step (x, y, E) (Step F n) = (x + n, y, E)
step (x, y, S) (Step F n) = (x, y + n, S)
step (x, y, W) (Step F n) = (x - n, y, W)

distance (x, y, _) = abs x + abs y

part1Solution :: Route -> Int
part1Solution = distance . foldl step (0, 0, N) . steps

babySteps :: Step -> [Step]
babySteps (Step d n) = Step d 1 : replicate (n - 1) (Step F 1)

part2Solution :: Route -> Maybe Int
part2Solution =
  fmap distance
    . fst
    . foldl
      ( \(r, cs) c@(x, y, _) ->
          if isJust r
            then (r, [])
            else if (x, y) `elem` cs then (Just c, []) else (Nothing, (x, y) : cs)
      )
      (Nothing, [])
    . scanl step (0, 0, N)
    . concatMap babySteps
    . steps
