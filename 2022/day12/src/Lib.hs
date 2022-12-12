module Lib
  ( part1Solution,
    part2Solution,
    parseHeights,
  )
where

import Control.Monad ((<=<))
import Data.List (elemIndex, foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

allHeights :: [Char]
allHeights = ['a' .. 'z']

charToInt :: Char -> Int
charToInt = fromMaybe 100 . (`elemIndex` allHeights)

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

data Heights = Heights {hS :: Point, hE :: Point, hMap :: M.Map Point (Int, Maybe Int)} deriving (Show)

parseHeights :: String -> Heights
parseHeights cs = Heights s e m
  where
    css = lines cs
    mapHeights :: (Int -> Int -> a) -> [a]
    mapHeights f = [f i j | i <- [0 .. w - 1], j <- [0 .. h - 1]]
    findPlace :: Char -> Point
    findPlace c =
      head . filter (\p -> pX p /= -1 && pY p /= -1) . mapHeights $
        ( \i j ->
            if css !! j !! i == c
              then Point i j
              else Point (-1) (-1)
        )
    h = length css
    w = length . head $ css
    s = findPlace 'S'
    e = findPlace 'E'
    m =
      M.fromList . mapHeights $
        ( \i j ->
            let c = css !! j !! i
                p = Point i j
             in if c == 'S'
                  then (p, (charToInt 'a', Nothing))
                  else
                    if c == 'E'
                      then (p, (charToInt 'z', Just 0))
                      else (p, (charToInt c, Nothing))
        )

minPathLengthes :: Heights -> Heights
minPathLengthes hs = doer (S.singleton (hE hs)) hs 1
  where
    dist p = snd . fromMaybe (-100, Nothing) . M.lookup p . hMap $ hs
    height p = fst . fromMaybe (-100, Nothing) . M.lookup p . hMap $ hs
    doer :: S.Set Point -> Heights -> Int -> Heights
    doer front hs' steps
      | null front = hs'
      | otherwise = doer front' hs'' (steps + 1)
      where
        (front', hs'') =
          foldl' (explore steps) (S.empty, hs')
            . concatMap
              ( \p ->
                  let h = height p
                   in filter
                        (\p' -> h - height p' <= 1 && (isNothing . dist) p')
                        [p {pX = pX p - 1}, p {pX = pX p + 1}, p {pY = pY p - 1}, p {pY = pY p + 1}]
              )
            . S.toList
            $ front
    explore :: Int -> (S.Set Point, Heights) -> Point -> (S.Set Point, Heights)
    explore steps (front, hs') p
      | (isJust . (snd <=< (`M.lookup` hMap hs'))) p = (front, hs')
      | p `S.member` front = (front, hs')
      | otherwise = (S.insert p front, hs' {hMap = M.update (\(h, _) -> Just (h, Just steps)) p . hMap $ hs'})

part1Solution :: Heights -> Maybe Int
part1Solution hs = snd <=< M.lookup (hS hs) . hMap . minPathLengthes $ hs

part2Solution :: Heights -> Maybe Int
part2Solution = fmap minimum . mapM snd . M.elems . M.filter (\(h, mbD) -> h == 0 && isJust mbD) . hMap . minPathLengthes
