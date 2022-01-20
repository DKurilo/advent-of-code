module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (guard)
import Data.Char (isDigit)
import Data.List (sortOn)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

type X = Int

type Y = Int

type Size = Int

type Used = Int

data Node = Node {nodeId :: (Int, Int), nodeSize :: Size, nodeUsed :: Used} deriving (Eq, Ord, Show)

instance Read Node where
  readPrec = do
    lift . P.string $ "/dev/grid/node-x"
    x <- readPrec
    lift . P.string $ "-y"
    y <- readPrec
    lift P.skipSpaces
    -- Size
    size <- readPrec
    lift . P.char $ 'T'
    lift P.skipSpaces
    -- Used
    used <- readPrec
    lift . P.char $ 'T'
    lift P.skipSpaces
    -- Avail
    lift . P.munch1 $ isDigit
    lift . P.char $ 'T'
    lift P.skipSpaces
    -- Use
    lift . P.munch1 $ isDigit
    lift . P.char $ '%'
    return $ Node (x, y) size used

newtype Grid = Grid {unGrid :: M.Map (X, Y) Node} deriving (Eq, Ord, Show)

instance Read Grid where
  readPrec = do
    lift . P.munch $ (/= '/')
    Grid . M.fromList
      <$> (lift . P.many1)
        ( do
            node <- PC.readPrec_to_P readPrec 0
            P.skipSpaces
            return (nodeId node, node)
        )

part1Solution :: Grid -> Int
part1Solution gr = length $ do
  let nodes = M.toList . unGrid $ gr
  let l = length nodes - 1
  i <- [0 .. l]
  j <- [0 .. l]
  guard $ i /= j
  let na = nodes !! i
  let nb = nodes !! j
  guard $ (nodeUsed . snd) na /= 0
  guard $ (nodeUsed . snd) na <= (nodeSize . snd) nb - (nodeUsed . snd) nb
  return (na, nb)

nextSteps :: ((Int, Int), (Int, Int)) -> Grid -> [(((Int, Int), (Int, Int)), Grid)]
nextSteps (eNCoord@(x, y), eNPrevCoord) gr = do
  enFrom <- maybeToList $ eNCoord `M.lookup` unGrid gr
  eNCoord' <- [(x -1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
  guard (eNCoord' /= eNPrevCoord)
  enTo <- maybeToList $ eNCoord' `M.lookup` unGrid gr
  guard (nodeSize enFrom >= nodeUsed enTo)
  return
    ( ( eNCoord',
        eNCoord
      ),
      Grid
        . M.insert eNCoord (Node (nodeId enTo) (nodeSize enFrom) (nodeUsed enTo))
        . M.insert eNCoord' (Node (nodeId enFrom) (nodeSize enTo) (nodeUsed enFrom))
        . unGrid
        $ gr
    )

score :: Int -> Int -> (((Int, Int), (Int, Int)), Grid) -> Int
score goalX goalY (((enX, enY), _), gr) = abs (enX - gnX) + abs (enY - gnY) + abs gnX + abs gnY + abs enX + abs enY
  where
    (gnX, gnY) = fst . head . filter ((== (goalX, goalY)) . nodeId . snd) . M.toList . unGrid $ gr

stepsToGetData :: Int -> Int -> Grid -> Int
stepsToGetData goalX goalY gr = doer 0 [((en, en), gr)]
  where
    en = fst . head . filter ((== 0) . nodeUsed . snd) . M.toList . unGrid $ gr
    doer :: Int -> [(((X, Y), (X, Y)), Grid)] -> Int
    doer steps front
      | null front = -1
      | any (\(_, gr') -> (fmap nodeId . M.lookup (0, 0) . unGrid) gr' == Just (goalX, goalY)) front = steps
      | otherwise = doer (steps + 1) . take 100 . sortOn (score goalX goalY) . concatMap (uncurry nextSteps) $ front

-- we considering it's not possible to copy data from two nodes to any other node
-- so each node contains data from some other node
-- and we have only one node that is empty, so this is just Fifteen puzzle
-- and solution is as for solution for Fifteen puzzle, we have score function and
-- sorting possible movements using this score function
-- I choose Manhattan distance for empty node + Manhattan distance for node we want
-- + Manhattan distance between empty and goal nodes.
-- It worked, but I'm always strugling with fifteen puzzle solution
-- https://github.com/DKurilo/tiles
part2Solution :: Grid -> Int
part2Solution gr = stepsToGetData x 0 gr
  where
    x = maximum . map fst . M.keys . unGrid $ gr
