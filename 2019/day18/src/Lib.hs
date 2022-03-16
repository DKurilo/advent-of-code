module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (first)
import Data.Char (isUpper, toLower)
import Data.Function (on)
import Data.List (foldl', intercalate)
import Data.Map (Map (..), fromList)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Ord (compare)
import Data.Set (Set (..))
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

newtype Key = Key Char deriving (Eq, Ord, Show)

newtype Door = Door Char deriving (Eq, Ord, Show)

data Tile = TDoor Door | TKey Key | TPassage deriving (Eq, Show)

getKey :: Tile -> Maybe Key
getKey (TKey k) = Just k
getKey _ = Nothing

getDoor :: Tile -> Maybe Door
getDoor (TDoor d) = Just d
getDoor _ = Nothing

keyForDoor :: Door -> Key
keyForDoor (Door c) = Key . toLower $ c

data Vault = Vault
  { vMap :: Map Point Tile,
    vKeys :: Map Key Point,
    vDoors :: Map Door Point,
    vBots :: [Point]
  }
  deriving (Show)

parseVault :: [String] -> Vault
parseVault =
  (\(vault, bots, keys, doors) -> Vault vault keys doors bots)
    . foldl' doer (M.empty, [], M.empty, M.empty)
    . zip [0 ..]
  where
    doer ::
      (Map Point Tile, [Point], Map Key Point, Map Door Point) ->
      (Int, String) ->
      (Map Point Tile, [Point], Map Key Point, Map Door Point)
    doer vm (y, cs) = foldl' doer' vm . zip [0 ..] $ cs
      where
        doer' ::
          (Map Point Tile, [Point], Map Key Point, Map Door Point) ->
          (Int, Char) ->
          (Map Point Tile, [Point], Map Key Point, Map Door Point)
        doer' (vault, bots, keys, doors) (x, c)
          | c == '#' = (vault, bots, keys, doors)
          | c == '.' = (M.insert point TPassage vault, bots, keys, doors)
          | c == '@' = (M.insert point TPassage vault, point : bots, keys, doors)
          | isUpper c = (M.insert point (TDoor door) vault, bots, keys, M.insert door point doors)
          | otherwise = (M.insert point (TKey key) vault, bots, M.insert key point keys, doors)
          where
            point = Point x y
            door = Door c
            key = Key c

pointsAround :: Point -> [Point]
pointsAround p = [p {pX = pX p + 1}, p {pX = pX p - 1}, p {pY = pY p + 1}, p {pY = pY p - 1}]

data Bots = Bots
  { botsPoints :: [Point],
    botsKeys :: Set Key,
    botsActive :: Int,
    botsAmount :: Int,
    botsCanSwitch :: Bool
  }
  deriving (Show, Eq, Ord)

collectAllKeys :: Vault -> Maybe Int
collectAllKeys vault = doer start start 0
  where
    start = S.singleton $ Bots (vBots vault) S.empty 0 ((length . vBots) vault) True
    doer :: Set Bots -> Set Bots -> Int -> Maybe Int
    doer front visited steps
      | S.null front = Nothing
      | (not . S.null) final = Just (steps + 1)
      | otherwise = doer front' visited' (steps + 1)
      where
        front' = S.fromList . filter (`S.notMember` visited) . concatMap posAround . S.toList $ front
        final = S.filter (\p -> (S.size . botsKeys) p == (M.size . vKeys) vault) front'
        visited' = visited `S.union` front'
        isKey :: Point -> Bool
        isKey p = case p `M.lookup` vMap vault of
          Just (TKey _) -> True
          _ -> False
        posAround :: Bots -> [Bots]
        posAround bots
          | botsCanSwitch bots = concatMap (\i -> posAroundActive $ bots {botsActive = i}) [0 .. botsAmount bots - 1]
          | otherwise = posAroundActive bots
        posAroundActive :: Bots -> [Bots]
        posAroundActive bots =
          map
            ( \p ->
                let (canSwitch, newKeys) = keys p
                 in Bots
                      ( map
                          ( \i ->
                              if i == botsActive bots
                                then p
                                else botsPoints bots !! i
                          )
                          [0 .. botsAmount bots - 1]
                      )
                      newKeys
                      (botsActive bots)
                      (botsAmount bots)
                      canSwitch
            )
            ps
          where
            activeBotPoint = botsPoints bots !! botsActive bots
            ps =
              filter
                ( \p -> case p `M.lookup` vMap vault of
                    Just (TDoor d)
                      | keyForDoor d `S.notMember` botsKeys bots -> False
                    Nothing -> False
                    _ -> True
                )
                . pointsAround
                $ activeBotPoint
            keys :: Point -> (Bool, Set Key)
            keys p = case p `M.lookup` vMap vault of
              Just (TKey k) -> (True, S.insert k . botsKeys $ bots)
              _ -> (False, botsKeys bots)

part1Solution :: [String] -> Maybe Int
part1Solution = collectAllKeys . parseVault

addBots :: [String] -> [String]
addBots css = [[replace (Point x y) (css !! y !! x) | x <- [0 .. (length . (!! y)) css - 1]] | y <- [0 .. length css - 1]]
  where
    vaultMap = [(Point x y, css !! y !! x) | y <- [0 .. length css - 1], x <- [0 .. (length . (!! y)) css - 1]]
    me = fst . head . filter ((== '@') . snd) $ vaultMap
    replace :: Point -> Char -> Char
    replace p c
      | p == me {pX = pX me - 1, pY = pY me - 1} = '@'
      | p == me {pX = pX me - 1, pY = pY me + 1} = '@'
      | p == me {pX = pX me + 1, pY = pY me - 1} = '@'
      | p == me {pX = pX me + 1, pY = pY me + 1} = '@'
      | abs (pX p - pX me) + abs (pY p - pY me) <= 1 = '#'
      | otherwise = c

part2Solution :: [String] -> Maybe Int
part2Solution = collectAllKeys . parseVault . addBots
