module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.Bifunctor (second)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data Dir
  = U
  | R
  | D
  | L
  deriving (Eq, Ord, Show)

data Point = P
  { px :: Int
  , py :: Int
  , pd :: Dir
  } deriving (Eq, Ord, Show)

move :: Point -> Point
move p =
  case pd p of
    U -> p {py = py p - 1}
    R -> p {px = px p + 1}
    D -> p {py = py p + 1}
    L -> p {px = px p - 1}

rotations :: Point -> [Point]
rotations p =
  case pd p of
    U -> [p {pd = L}, p {pd = R}]
    R -> [p {pd = U}, p {pd = D}]
    D -> [p {pd = L}, p {pd = R}]
    L -> [p {pd = U}, p {pd = D}]

data Maze = Maze
  { mzGraph :: M.Map Point [(Point, Int)]
  , mzStart :: Point
  , mzEnd :: [Point]
  , mzRevGraph :: M.Map Point [(Point, Int)]
  } deriving (Show)

parseMaze :: [String] -> Maze
parseMaze css = Maze graph start end reversedGraph
  where
    w = length . head $ css
    h = length css
    findCells :: Char -> [(Int, Int)]
    findCells c =
      [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1], css !! y !! x == c]
    findUniq :: Char -> (Int, Int)
    findUniq = head . findCells
    start = (\(x, y) -> P x y R) . findUniq $ 'S'
    (endCoordX, endCoordY) = findUniq 'E'
    end = fmap (P endCoordX endCoordY) [U, L, D, R]
    emptyCells = findCells '.' <> findCells 'E' <> findCells 'S'
    mazeEmpt = S.fromList emptyCells
    ways :: Point -> [(Point, [(Point, Int)])]
    ways p =
      [(p, [(p', 1)]) | let p' = move p, (px p', py p') `S.member` mazeEmpt]
        <> (fmap (\p' -> (p, [(p', 1000)])) . rotations) p
    graph =
      M.fromListWith (<>)
        . concatMap ways
        . concatMap (\(x, y) -> [P x y U, P x y R, P x y D, P x y L])
        $ emptyCells
    reversedGraph =
      M.fromListWith (<>)
        . concatMap (\(p, pcs) -> fmap (\(p', c) -> (p', [(p, c)])) pcs)
        . M.toList
        $ graph

findShortest :: Maze -> (Int, M.Map Point Int)
findShortest maze = doer start start
  where
    start = (`M.singleton` 0) . mzStart $ maze
    doer :: M.Map Point Int -> M.Map Point Int -> (Int, M.Map Point Int)
    doer front visited
      | M.null front =
        ( minimum . fmap snd . filter ((`elem` mzEnd maze) . fst) . M.toList
            $ visited
        , visited)
      | otherwise = doer front' (front' `M.union` visited)
      where
        front' =
          foldl'
            (\ps (p, cost) ->
               M.unionWith min ps
                 . M.fromListWith min
                 . filter
                     (\(p', cost') ->
                        case p' `M.lookup` visited of
                          Just cost'' -> cost' < cost''
                          _ -> True)
                 . fmap (second (cost +))
                 . fromMaybe []
                 . (`M.lookup` mzGraph maze)
                 $ p)
            M.empty
            . M.toList
            $ front

part1Solution :: [String] -> Int
part1Solution = fst . findShortest . parseMaze

tilesOnShortestPath :: Maze -> Int -> M.Map Point Int -> Int
tilesOnShortestPath maze cost visited =
  S.size . S.map (\p -> (px p, py p)) . doer $ ends
  where
    ends =
      S.fromList
        . fmap fst
        . filter (\(p, cost') -> p `elem` mzEnd maze && cost' == cost)
        . M.toList
        $ visited
    doer :: S.Set Point -> S.Set Point
    doer ps
      | S.null ps' = ps
      | otherwise = S.union ps (doer ps')
      where
        ps' =
          S.fromList
            . concatMap
                (\p ->
                   let cost' = fromMaybe 0 . M.lookup p $ visited
                       pcs = fromMaybe [] . M.lookup p . mzRevGraph $ maze
                    in if cost' == 0
                         then []
                         else fmap fst
                                . filter
                                    (\(p', cost'') ->
                                       (cost' - cost'')
                                         == (fromMaybe 0 . M.lookup p') visited)
                                $ pcs)
            . S.toList
            $ ps

part2Solution :: [String] -> Int
part2Solution css = tilesOnShortestPath maze cost visited
  where
    maze = parseMaze css
    (cost, visited) = findShortest maze
