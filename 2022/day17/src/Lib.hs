module Lib
  ( part1Solution,
    part2Solution,
    charToAir,
  )
where

import Data.List (elemIndex, intercalate)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

newtype Cave = Cave {unCave :: S.Set Point}

instance Show Cave where
  show cave =
    intercalate
      "\n"
      [ [ if Point x y `S.member` unCave cave
            then '#'
            else '.'
          | x <- [0 .. 6]
        ]
        | y <- [top, top - 1 .. 0]
      ]
    where
      top = topOfCave cave

data Air = ALeft | ARight deriving (Eq, Show)

charToAir :: Char -> Air
charToAir '<' = ALeft
charToAir _ = ARight

type Rock = S.Set Point

rock1 :: Rock
rock1 = S.fromList [Point 0 0, Point 1 0, Point 2 0, Point 3 0]

rock2 :: Rock
rock2 = S.fromList [Point 1 2, Point 0 1, Point 1 1, Point 2 1, Point 1 0]

rock3 :: Rock
rock3 = S.fromList [Point 2 2, Point 2 1, Point 0 0, Point 1 0, Point 2 0]

rock4 :: Rock
rock4 = S.fromList [Point 0 3, Point 0 2, Point 0 1, Point 0 0]

rock5 :: Rock
rock5 = S.fromList [Point 0 1, Point 1 1, Point 0 0, Point 1 0]

rocks :: [Rock]
rocks = cycle [rock1, rock2, rock3, rock4, rock5]

topOfCave :: Cave -> Int
topOfCave = fromMaybe 0 . S.lookupMax . S.map pY . unCave

addRockToCave :: Rock -> Point -> Cave -> Cave
addRockToCave rock p = Cave . S.union (S.map (\p' -> Point (pX p + pX p') (pY p + pY p')) rock) . unCave

dropRock :: Cave -> [Air] -> Int -> Int -> Rock -> Point -> (Cave, [Air], Int, Point)
dropRock cave airs n i rock p
  | canMoveDown = dropRock cave (tail airs) n i' rock (p' {pY = pY p' - 1})
  | otherwise = (addRockToCave rock p' cave, tail airs, i', p')
  where
    i' = (i + 1) `mod` n
    canMoveLeft =
      all
        ( \p'' ->
            let moved = (p'' {pX = pX p'' - 1 + pX p, pY = pY p'' + pY p})
             in (S.notMember moved . unCave) cave && pX moved >= 0
        )
        . S.toList
        $ rock
    canMoveRight =
      all
        ( \p'' ->
            let moved = (p'' {pX = pX p'' + 1 + pX p, pY = pY p'' + pY p})
             in (S.notMember moved . unCave) cave && pX moved < 7
        )
        . S.toList
        $ rock
    p'
      | head airs == ALeft && canMoveLeft = p {pX = pX p - 1}
      | head airs == ARight && canMoveRight = p {pX = pX p + 1}
      | otherwise = p
    canMoveDown =
      all
        ( \p'' ->
            let moved = p'' {pX = pX p'' + pX p', pY = pY p'' - 1 + pY p'}
             in (S.notMember moved . unCave) cave && pY moved > 0
        )
        . S.toList
        $ rock

rawDropEmAll :: [Air] -> Int -> [(Cave, [Air], Int, [Rock], Int, Point)]
rawDropEmAll airs n =
  iterate
    ( \(cave, airs', i, rocks', j, _) ->
        let top = topOfCave cave
            (cave', airs'', i', p') = dropRock cave airs' n i (head rocks') (Point 2 (top + 4))
         in (cave', airs'', i', tail rocks', (j + 1) `mod` 5, p')
    )
    (Cave S.empty, airs, 0, rocks, 0, Point 0 0)

dropEmAll :: [Air] -> Int -> [Cave]
dropEmAll airs = map (\(cave, _, _, _, _, _) -> cave) . rawDropEmAll airs

part1Solution :: [Air] -> Int
part1Solution airs = topOfCave . (!! 2022) . dropEmAll (cycle airs) . length $ airs

part2Solution :: [Air] -> Int
part2Solution airs = startHeight + repeats * repHeight
  where
    allCaves = rawDropEmAll (cycle airs) . length $ airs
    caves = map (\(cave, _, _, _, _, _) -> cave) allCaves
    nijs = zipWith (\n' (_, _, i, _, j, p) -> (n', (i, j, pX p))) [0 ..] allCaves
    ijs = map snd nijs
    -- It's not enough to start from the same rock and hot air flow because each rock can end up in different position
    -- So we looking for exact rock in exact location that make it not possible for the next rock to fall through
    (n, _) = head . filter (\(n', ij@(_, j, x)) -> ij `elem` take n' ijs && j == 1 && x == 1) $ nijs
    nFirst = fromMaybe 0 $ ijs !! n `elemIndex` ijs
    nRep = n - nFirst
    nStartRep = nFirst + ((1000000000000 - nFirst) `mod` (n - nFirst))
    startHeight = topOfCave . (!! nStartRep) $ caves
    repHeight = (topOfCave . (!! (nStartRep + nRep))) caves - (topOfCave . (!! nStartRep)) caves
    repeats = (1000000000000 - nStartRep) `div` nRep
