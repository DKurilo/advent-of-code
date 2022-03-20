{-# LANGUAGE LambdaCase #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set (..))
import qualified Data.Set
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Show, Eq, Ord)

pointsAround :: Point -> [Point]
pointsAround p = [p {pX = pX p + 1}, p {pX = pX p - 1}, p {pY = pY p + 1}, p {pY = pY p - 1}]

data PortalType = PTInner | PTOuter deriving (Show, Eq)

data Tile = TPassage Point | TPortal Point PortalType String deriving (Show, Eq)

tilePoint :: Tile -> Point
tilePoint (TPassage p) = p
tilePoint (TPortal p _ _) = p

data Donut = Donut {dGraph :: Map Point [Tile], dEnter :: Point, dExit :: Point} deriving (Show, Eq)

parseDonut :: [String] -> Donut
parseDonut css = Donut (M.fromList . map (\p -> (p, getAround p)) $ (enter : exit : passages)) enter exit
  where
    maxY = length css - 1
    maxX = (length . head) css - 1
    points = concat . zipWith (\y -> zipWith (\x c -> (Point x y, c)) [0 ..]) [0 ..] $ css
    psm = M.fromList points
    getChar p = fromMaybe '#' . M.lookup p $ psm
    passages = map fst . filter ((== '.') . snd) $ points
    passagesMap = S.fromList passages
    getPortal :: Point -> ((Point, Point), String)
    getPortal p =
      head
        [ ((p, p'), [c1, c2])
          | (p', p1, p2) <-
              [ (p {pX = pX p - 1}, p {pX = pX p - 2}, p {pX = pX p - 1}),
                (p {pX = pX p + 1}, p {pX = pX p + 1}, p {pX = pX p + 2}),
                (p {pY = pY p - 1}, p {pY = pY p - 2}, p {pY = pY p - 1}),
                (p {pY = pY p + 1}, p {pY = pY p + 1}, p {pY = pY p + 2})
              ],
            let c1 = getChar p1,
            isAlpha c1,
            let c2 = getChar p2,
            isAlpha c2
        ]
    portals =
      map getPortal
        . filter
          ( any
              ( \p -> case p `M.lookup` psm of
                  Just c
                    | isAlpha c -> True
                  _ -> False
              )
              . pointsAround
          )
        $ passages
    portalsOutIn = M.fromList portals
    portalsIn = M.fromList . map (\((_, p), cs) -> (p, cs)) $ portals
    portalsOut = M.fromList . map (\((p, _), cs) -> (p, cs)) $ portals
    enter = fst . head . M.toList . M.filter (== "AA") $ portalsOut
    exit = fst . head . M.toList . M.filter (== "ZZ") $ portalsOut
    pointToTile :: Point -> Maybe Tile
    pointToTile p = case p `M.lookup` portalsIn of
      Just cs
        | cs == "AA" -> Nothing
        | cs == "ZZ" -> Nothing
        | otherwise -> Just $ TPortal portalPoint portalType cs
        where
          portalPoint = fst . head . filter ((/= p) . snd) . M.keys . M.filter (== cs) $ portalsOutIn
          portalType
            | pX portalPoint == 2 || pY portalPoint == 2 = PTInner
            | pX portalPoint == maxX - 2 || pY portalPoint == maxY - 2 = PTInner
            | otherwise = PTOuter
      _
        | p `S.member` passagesMap -> Just $ TPassage p
        | otherwise -> Nothing
    getAround :: Point -> [Tile]
    getAround = map (\(Just x) -> x) . filter isJust . map pointToTile . pointsAround

shortestWayLengthBetweenPoints :: Donut -> Point -> Point -> Maybe Int
shortestWayLengthBetweenPoints donut start end = doer startSet startSet
  where
    startSet = S.singleton start
    doer :: Set Point -> Set Point -> Maybe Int
    doer visited front
      | S.null front = Nothing
      | end `S.member` front = Just 0
      | otherwise = fmap (1 +) . doer visited' $ front'
      where
        around p = case p `M.lookup` dGraph donut of
          Just tiles -> map tilePoint tiles
          _ -> []
        front' = S.fromList . filter (`S.notMember` visited) . concatMap around . S.toList $ front
        visited' = visited `S.union` front'

shortestWayLength :: Donut -> Maybe Int
shortestWayLength donut = shortestWayLengthBetweenPoints donut (dEnter donut) (dExit donut)

part1Solution :: [String] -> Maybe Int
part1Solution = shortestWayLength . parseDonut

maxLevel = 10000

shortestWayLengthBetweenPoints' :: Donut -> (Point, Int) -> (Point, Int) -> Maybe Int
shortestWayLengthBetweenPoints' donut start end = doer startSet startSet
  where
    startSet = S.singleton start
    doer :: Set (Point, Int) -> Set (Point, Int) -> Maybe Int
    doer visited front
      | S.null front = Nothing
      | end `S.member` front = Just 0
      | otherwise = fmap (1 +) . doer visited' $ front'
      where
        around (p, l)
          | l >= maxLevel = []
          | otherwise = case p `M.lookup` dGraph donut of
            Just tiles ->
              concatMap
                ( \case
                    TPassage p -> [(p, l)]
                    TPortal p PTInner _ -> [(p, l + 1)]
                    TPortal p PTOuter _
                      | l == 0 -> []
                      | otherwise -> [(p, l - 1)]
                )
                tiles
            _ -> []
        front' = S.fromList . filter (`S.notMember` visited) . concatMap around . S.toList $ front
        visited' = visited `S.union` front'

shortestWayLength' :: Donut -> Maybe Int
shortestWayLength' donut = shortestWayLengthBetweenPoints' donut (dEnter donut, 0) (dExit donut, 0)

part2Solution :: [String] -> Maybe Int
part2Solution = shortestWayLength' . parseDonut
