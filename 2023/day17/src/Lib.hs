module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (second)
import Data.List (elemIndex, foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)

data Point = P {px :: Int, py :: Int} deriving (Show, Eq, Ord)

data Direction = DUp | DRight | DDown | DLeft deriving (Show, Eq, Ord)

allDirections :: [Direction]
allDirections = [DUp, DRight, DDown, DLeft]

pointInDirection :: Point -> Direction -> Int -> Point
pointInDirection p DUp n = p {py = py p - n}
pointInDirection p DRight n = p {px = px p + n}
pointInDirection p DDown n = p {py = py p + n}
pointInDirection p DLeft n = p {px = px p - n}

opposite :: Direction -> Direction
opposite DUp = DDown
opposite DLeft = DRight
opposite DDown = DUp
opposite DRight = DLeft

parseMap :: [String] -> (M.Map Point Int, (Int, Int))
parseMap css =
  ( M.fromList
      [ (P x y, n)
        | x <- [0 .. w - 1],
          y <- [0 .. h - 1],
          let n = fromMaybe 0 . (`elemIndex` "01234567890") . (!! x) . (!! y) $ css
      ],
    (w, h)
  )
  where
    w = length . head $ css
    h = length css

mkGraph :: Int -> Int -> (M.Map Point Int, (Int, Int)) -> (Point, M.Map (Point, Direction) [((Point, Direction), Int)])
mkGraph minMove maxMove (m, (w, h)) =
  ( P (w - 1) (h - 1),
    M.fromList $
      [ ((P x y, d), allWaysOut (P x y) d)
        | x <- [0 .. w - 1],
          y <- [0 .. h - 1],
          d <- allDirections
      ]
  )
  where
    allWaysOut :: Point -> Direction -> [((Point, Direction), Int)]
    allWaysOut p d =
      fmap (second (fromMaybe 10000000))
        . filter (isJust . snd)
        $ [ ((p', d'), loss)
            | n <- [minMove .. maxMove],
              d' <- filter (\d'' -> d'' /= d && d'' /= opposite d) allDirections,
              let p' = pointInDirection p d' n
                  loss = fmap sum . sequence $ [(`M.lookup` m) . pointInDirection p d' $ i | i <- [1 .. n]]
          ]

lossOnTheBestWay :: Point -> Point -> M.Map (Point, Direction) [((Point, Direction), Int)] -> Int
lossOnTheBestWay start end g = minimum . fmap (fromMaybe (-1)) . filter isJust $ [(end, d) `M.lookup` visitedMap | d <- allDirections]
  where
    visitedMap = doer [((start, d), 0) | d <- allDirections] (M.fromList [((start, d), 0) | d <- allDirections])

    doer :: [((Point, Direction), Int)] -> M.Map (Point, Direction) Int -> M.Map (Point, Direction) Int
    doer front visited
      | null front = visited
      | otherwise = doer goodToGo visited'
      where
        canGo =
          concatMap
            ( \(pd, loss) -> case pd `M.lookup` g of
                Just pdls -> fmap (second ((+) loss)) pdls
                _ -> []
            )
            front

        (goodToGo, visited') =
          foldl'
            ( \(pdls, visited'') (pd, l) -> case pd `M.lookup` visited'' of
                Just l'
                  | l >= l' -> (pdls, visited'')
                _ -> ((pd, l) : pdls, M.insert pd l visited'')
            )
            ([], visited)
            canGo

part1Solution :: [String] -> Int
part1Solution = uncurry (lossOnTheBestWay (P 0 0)) . mkGraph 1 3 . parseMap

part2Solution :: [String] -> Int
part2Solution = uncurry (lossOnTheBestWay (P 0 0)) . mkGraph 4 10 . parseMap
