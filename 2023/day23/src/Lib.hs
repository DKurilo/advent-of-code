module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (second)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data Point = P {px :: Int, py :: Int} deriving (Show, Eq, Ord)

data Direction = DUp | DRight | DDown | DLeft deriving (Show)

data Tile = TEmpty | TSlopeUp | TSlopeRight | TSlopeDown | TSlopeLeft deriving (Show)

charToTile :: Char -> Tile
charToTile '.' = TEmpty
charToTile '^' = TSlopeUp
charToTile '>' = TSlopeRight
charToTile 'v' = TSlopeDown
charToTile '<' = TSlopeLeft
charToTile c = error ("wrong character" <> [c])

nextPossibleDirections :: Tile -> [Direction]
nextPossibleDirections TEmpty = [DUp, DLeft, DDown, DRight]
nextPossibleDirections TSlopeUp = [DUp]
nextPossibleDirections TSlopeRight = [DRight]
nextPossibleDirections TSlopeDown = [DDown]
nextPossibleDirections TSlopeLeft = [DLeft]

stepInDirection :: Direction -> Point -> Point
stepInDirection DUp p = p {py = py p - 1}
stepInDirection DRight p = p {px = px p + 1}
stepInDirection DDown p = p {py = py p + 1}
stepInDirection DLeft p = p {px = px p - 1}

mkTrial :: [String] -> M.Map Point Tile
mkTrial =
  M.fromList
    . fmap (second charToTile)
    . filter ((/= '#') . snd)
    . concat
    . zipWith (\y -> zipWith (\x c -> (P x y, c)) [0 ..]) [0 ..]

findPathsLengths :: M.Map Point Tile -> Point -> Point -> (Tile -> [Direction]) -> [Int]
findPathsLengths trial start finish getDirections = doer start (getDirections TEmpty) vstart 0
  where
    vstart = S.singleton start

    doer :: Point -> [Direction] -> S.Set Point -> Int -> [Int]
    doer p ds visited steps
      | finish == p = [steps]
      | otherwise =
          foldl'
            ( \xs p' ->
                let ds' = foldMap getDirections . M.lookup p' $ trial
                    v' = S.insert p' visited
                    xs' = doer p' ds' v' (steps + 1)
                 in xs <> xs'
            )
            []
            . filter
              ( \p' ->
                  p' `M.member` trial
                    && p' `S.notMember` visited
              )
            . fmap (\d -> stepInDirection d p)
            $ ds

part1Solution :: [String] -> Int
part1Solution css = maximum . findPathsLengths trial start finish $ nextPossibleDirections
  where
    yEnd = length css - 1
    trial = mkTrial css
    start = firstSpace 0
    finish = firstSpace yEnd

    firstSpace y =
      fst
        . head
        . filter snd
        . fmap
          ( \x ->
              let p = P x y
               in (p, p `M.member` trial)
          )
        $ [0 ..]

mkGraph :: M.Map Point Tile -> M.Map Point [(Point, Int)]
mkGraph ts = M.fromList [(v, neighbors v) | v <- vertices]
  where
    vertices = filter isVertex . M.keys $ ts

    neighbors :: Point -> [(Point, Int)]
    neighbors p = fmap (\p' -> vertexInDirection p p' 1) . closeNeighbors $ p

    vertexInDirection :: Point -> Point -> Int -> (Point, Int)
    vertexInDirection p1 p2 w
      | null vs = (p2, w)
      | length vs > 1 = (p2, w)
      | otherwise = vertexInDirection p2 (head vs) (w + 1)
      where
        vs = filter (\p -> p /= p1) . closeNeighbors $ p2

    isVertex :: Point -> Bool
    isVertex p = cnsLength > 2 || cnsLength == 1
      where
        cnsLength = (length . closeNeighbors) p

    closeNeighbors :: Point -> [Point]
    closeNeighbors p = [p' | d <- nextPossibleDirections TEmpty, let p' = stepInDirection d p, p' `M.member` ts]

findPathsLengths2 :: M.Map Point [(Point, Int)] -> Point -> Point -> [Int]
findPathsLengths2 graph start finish = doer start (S.singleton start) 0
  where
    doer :: Point -> S.Set Point -> Int -> [Int]
    doer p visited steps
      | p == finish = [steps]
      | null pws = []
      | otherwise = concatMap (\(p', w) -> doer p' (S.insert p' visited) (steps + w)) pws
      where
        pws = filter ((`S.notMember` visited) . fst) . fromMaybe [] . M.lookup p $ graph

part2Solution :: [String] -> Int
part2Solution css = maximum . findPathsLengths2 graph start $ finish
  where
    yEnd = length css - 1
    trial = mkTrial css
    start = firstSpace 0
    finish = firstSpace yEnd

    graph = mkGraph trial

    firstSpace y =
      fst
        . head
        . filter snd
        . fmap
          ( \x ->
              let p = P x y
               in (p, p `M.member` trial)
          )
        $ [0 ..]
