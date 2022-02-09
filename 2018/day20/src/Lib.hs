module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', intercalate)
import Data.Map (Map (..))
import qualified Data.Map.Strict as M
import Data.Set (Set (..))
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Regex = N | E | W | S | Seq [Regex] | Par [Regex] | Empty deriving (Eq, Show)

instance Read Regex where
  readPrec =
    lift $
      (N <$ P.char 'N')
        P.+++ (E <$ P.char 'E')
        P.+++ (W <$ P.char 'W')
        P.+++ (S <$ P.char 'S')
        P.+++ ( do
                  P.char '^'
                  rs <- P.many $ PC.readPrec_to_P readPrec 0
                  P.char '$'
                  return $ Seq rs
              )
        P.+++ ( do
                  P.char '('
                  r <- Seq <$> P.many (PC.readPrec_to_P readPrec 0)
                  rs <-
                    P.many $
                      ( do
                          P.char '|'
                          Seq <$> P.many (PC.readPrec_to_P readPrec 0)
                      )
                        P.<++ (Empty <$ P.char '|')
                  P.char ')'
                  return $ Par (r : rs)
              )

data Room = Room {rX :: Int, rY :: Int} deriving (Eq, Ord, Show)

newtype BaseMap = BaseMap {bmDoors :: Map Room (Set Room)} deriving (Eq, Show)

moveBaseMap :: Int -> Int -> BaseMap -> BaseMap
moveBaseMap dx dy = BaseMap . M.map (S.mapMonotonic moveRoom) . M.mapKeysMonotonic moveRoom . bmDoors
  where
    moveRoom r = r {rX = rX r + dx, rY = rY r + dy}

joinBaseMaps :: BaseMap -> BaseMap -> BaseMap
joinBaseMaps bm1 bm2 = BaseMap (M.unionWith S.union (bmDoors bm1) (bmDoors bm2))

emptyBaseMap = BaseMap M.empty

addRoom :: Room -> Room -> BaseMap -> (BaseMap, Room)
addRoom room room' bm =
  ( bm
      { bmDoors = M.insertWith S.union room (S.singleton room') . M.insertWith S.union room' (S.singleton room) . bmDoors $ bm
      },
    room'
  )

processRegex :: Room -> BaseMap -> Regex -> (BaseMap, Room)
processRegex room bm N = addRoom room (room {rY = rY room - 1}) bm
processRegex room bm E = addRoom room (room {rX = rX room + 1}) bm
processRegex room bm W = addRoom room (room {rX = rX room - 1}) bm
processRegex room bm S = addRoom room (room {rY = rY room + 1}) bm
processRegex room bm Empty = (bm, room)
processRegex room bm (Seq []) = (bm, room)
processRegex room bm (Seq (r : rs)) = case r of
  Par rs' -> (foldl' processPath bm rs', room)
    where
      (bm', _) = processRegex (Room 0 0) emptyBaseMap (Seq rs)
      processPath :: BaseMap -> Regex -> BaseMap
      processPath bm'' r' = joinBaseMaps bm''' (moveBaseMap (rX room') (rY room') bm')
        where
          (bm''', room') = processRegex room bm'' r'
  _ -> processRegex room' bm' (Seq rs)
    where
      (bm', room') = processRegex room bm r
processRegex room bm (Par regexes) = (foldl' (\bm' -> fst . processRegex room bm') bm regexes, room)

mkBaseMap :: Regex -> BaseMap
mkBaseMap = fst . processRegex (Room 0 0) emptyBaseMap

showBaseMap :: BaseMap -> String
showBaseMap bm = intercalate "\n" [[showChar x y | x <- [0 .. 2 * dx + 2]] | y <- [0 .. 2 * dy + 2]]
  where
    allRooms = M.keysSet . bmDoors $ bm
    xs = S.map rX allRooms
    ys = S.map rY allRooms
    minX = S.findMin xs
    maxX = S.findMax xs
    minY = S.findMin ys
    maxY = S.findMax ys
    dx = maxX - minX
    dy = maxY - minY
    showChar x y
      | even x && even y = '#'
      | even x = case Room ((x - 2) `div` 2 + minX) ((y - 1) `div` 2 + minY) `M.lookup` bmDoors bm of
        Just rs -> if Room (x `div` 2 + minX) ((y - 1) `div` 2 + minY) `S.member` rs then '|' else '#'
        _ -> '#'
      | even y = case Room ((x - 1) `div` 2 + minX) ((y - 2) `div` 2 + minY) `M.lookup` bmDoors bm of
        Just rs -> if Room ((x - 1) `div` 2 + minX) (y `div` 2 + minY) `S.member` rs then '-' else '#'
        _ -> '#'
      | (x - 1) `div` 2 + minX == 0 && (y - 1) `div` 2 + minY == 0 = 'X'
      | otherwise = if Room ((x - 1) `div` 2 + minX) ((y - 1) `div` 2 + minY) `S.member` allRooms then '.' else '#'

furthestRoom :: Room -> BaseMap -> (Map Room Int, Int)
furthestRoom room bm = doer 0 (M.singleton room 0) (S.singleton room)
  where
    doer :: Int -> Map Room Int -> Set Room -> (Map Room Int, Int)
    doer step visited front
      | S.null front' = (visited', step)
      | otherwise = doer (step + 1) visited' front'
      where
        front' =
          S.fromList
            . concatMap
              ( \room' -> case room' `M.lookup` bmDoors bm of
                  Just rooms -> filter (`M.notMember` visited) . S.toList $ rooms
                  _ -> []
              )
            . S.toList
            $ front
        visited' = visited `M.union` M.fromSet (const (step + 1)) front'

part1Solution :: Regex -> Int
part1Solution = snd . furthestRoom (Room 0 0) . mkBaseMap

part2Solution :: Regex -> Int
part2Solution = M.size . M.filter (>= 1000) . fst . furthestRoom (Room 0 0) . mkBaseMap
