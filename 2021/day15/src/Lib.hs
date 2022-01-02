module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import Data.Hashable (Hashable)
import Data.List (foldl', replicate, transpose)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.PQueue.Min as PQ
import qualified Data.PQueue.Prio.Min as PQM
import qualified Data.Set as S

type Matrix = [[Int]]

mkGraph' :: [[Int]] -> Matrix
mkGraph' xss =
  [ [ weight d x' y' | y' <- [0 .. yMax], x' <- [0 .. xMax], let d = abs (x - x') + abs (y - y')
    ]
    | y <- [0 .. yMax],
      x <- [0 .. xMax]
  ]
  where
    yMax = length xss - 1
    xMax = (length . head) xss - 1
    weight d x y
      | d == 0 = 0
      | d == 1 = xss !! y !! x
      | otherwise = maxBound

mulMatrix2 :: Matrix -> Matrix
mulMatrix2 m =
  [ [ minimum $ zipWith (\v v' -> if v < maxBound && v' < maxBound then v + v' else maxBound) c c'
      | c <- m
    ]
    | c' <- transpose m
  ]

part1Solution' :: [[Int]] -> Int
part1Solution' xss = last . head . (!! 14) . iterate mulMatrix2 $ g
  where
    maxY = length xss
    maxX = (length . head) xss
    g = mkGraph' xss

-- https://gist.github.com/abhin4v/8172534
-- https://en.wikipedia.org/wiki/A*_search_algorithm
-- I feel stupid. I did a lot of exercises using A* and here I just forgot it. AAAA!
astarSearch :: (Eq a, Hashable a) => a -> (a -> Bool) -> (a -> [(a, Int)]) -> (a -> Int) -> Maybe (Int, [a])
astarSearch startNode isGoalNode nextNodeFn heuristic =
  astar
    (PQM.singleton (heuristic startNode) (startNode, 0))
    Set.empty
    (Map.singleton startNode 0)
    Map.empty
  where
    astar pq seen gscore tracks
      | PQM.null pq = Nothing
      | isGoalNode node = Just (gcost, findPath tracks node)
      | Set.member node seen = astar pq' seen gscore tracks
      | otherwise = astar pq'' seen' gscore' tracks'
      where
        (node, gcost) = snd . PQM.findMin $ pq
        pq' = PQM.deleteMin pq
        seen' = Set.insert node seen
        successors =
          filter
            ( \(s, g, _) ->
                not (Set.member s seen')
                  && (not (s `Map.member` gscore) || g < (fromJust . Map.lookup s $ gscore))
            )
            $ successorsAndCosts node gcost
        pq'' = foldl' (\q (s, g, h) -> PQM.insert (g + h) (s, g) q) pq' successors
        gscore' = foldl' (\m (s, g, _) -> Map.insert s g m) gscore successors
        tracks' = foldl' (\m (s, _, _) -> Map.insert s node m) tracks successors
    successorsAndCosts node gcost = map (\(s, g) -> (s, gcost + g, heuristic s)) . nextNodeFn $ node
    findPath tracks node =
      if Map.member node tracks
        then findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
        else [node]

type Point = (Int, Int)

type Graph = Map.HashMap Point [(Point, Int)]

-- my not generic and a bit stupid implementation of A*.
-- it's possible to make it faster by moving visited from HashMap to mutable vector
astar :: (Point -> [(Point, Int)]) -> Point -> Point -> Maybe Int
astar nextNodes pStart pEnd = doer (PQ.singleton (pStart, 0)) (Map.singleton pStart 0)
  where
    doer :: PQ.MinQueue (Point, Int) -> Map.HashMap Point Int -> Maybe Int
    doer pvs visited
      | PQ.null pvs = pEnd `Map.lookup` visited
      | otherwise = doer ps' visited'
      where
        (ps', visited') = PQ.foldlU applyFront (PQ.empty, visited) pvs
        applyFront (ps'', visited'') (pFrom, vFrom) = (ps''', visited''')
          where
            pvs' = nextNodes pFrom
            (ps''', visited''') = foldl' applyNode (ps'', visited'') pvs'
            applyNode (ps, visited) (p, v) =
              case p `Map.lookup` visited of
                Just r
                  | r' < r -> psvs
                  | otherwise -> (ps, visited)
                _ -> psvs
              where
                r' = vFrom + v
                psvs = ((p, r') `PQ.insert` ps, Map.insert p r' visited)

mkGraph :: [[Int]] -> Graph
mkGraph xss =
  Map.fromListWith (++) . concat $
    [ [ weight d x y x' y' | y' <- [0 .. yMax], x' <- [0 .. xMax], let d = abs (x - x') + abs (y - y'), d == 1
      ]
      | y <- [0 .. yMax],
        x <- [0 .. xMax]
    ]
  where
    yMax = length xss - 1
    xMax = (length . head) xss - 1
    weight d x y x' y' = ((x, y), [((x', y'), xss !! y' !! x')])

part1Solution'' :: [[Int]] -> Int
part1Solution'' xss = fst . fromMaybe (-1, []) $ astarSearch (0, 0) (== (maxX, maxY)) nextNode (const 0)
  where
    maxY = length xss - 1
    maxX = (length . head) xss - 1
    g = mkGraph xss
    nextNode p = fromMaybe [] $ p `Map.lookup` g

part1Solution :: [[Int]] -> Int
part1Solution xss = fromMaybe (-1) $ astar nextNode (0, 0) (maxX, maxY)
  where
    maxY = length xss - 1
    maxX = (length . head) xss - 1
    g = mkGraph xss
    nextNode p = fromMaybe [] $ p `Map.lookup` g

part2Solution' :: [[Int]] -> Int
part2Solution' xss = fst . fromMaybe (-1, []) $ astarSearch (0, 0) (== (maxX, maxY)) nextNode (const 0)
  where
    maxY = length xss * 5 - 1
    maxX = (length . head) xss * 5 - 1
    calcRisk d v = (d + v - 1) `mod` 9 + 1
    xss' =
      concat
        . zipWith (map . map . calcRisk) [0 ..]
        . replicate 5
        . map (concat . zipWith (map . calcRisk) [0 ..] . replicate 5)
        $ xss
    g = mkGraph xss'
    nextNode p = fromMaybe [] $ p `Map.lookup` g

part2Solution :: [[Int]] -> Int
part2Solution xss = fromMaybe (-1) $ astar nextNode (0, 0) (maxX, maxY)
  where
    maxY = length xss * 5 - 1
    maxX = (length . head) xss * 5 - 1
    calcRisk d v = (d + v - 1) `mod` 9 + 1
    xss' =
      concat
        . zipWith (map . map . calcRisk) [0 ..]
        . replicate 5
        . map (concat . zipWith (map . calcRisk) [0 ..] . replicate 5)
        $ xss
    g = mkGraph xss'
    nextNode p = fromMaybe [] $ p `Map.lookup` g
