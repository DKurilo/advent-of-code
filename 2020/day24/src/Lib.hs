module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import qualified Data.Set                     as S
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

data Tile = Bl | Wh deriving (Show, Eq)

data Direction = E | SE | SW | W | NW | NE deriving (Show, Eq)

instance Read Direction where
    readPrec = (do
        lift $ P.string "se"
        return SE) <|> (do
        lift $ P.string "sw"
        return SW) <|> (do
        lift $ P.string "ne"
        return NE) <|> (do
        lift $ P.string "nw"
        return NW) <|> (do
        lift $ P.char 'e'
        return E) <|> (do
        lift $ P.char 'w'
        return W)

pDirections :: ReadPrec [Direction]
pDirections = do
    d <- readPrec
    ds <- pDirections <|> return []
    return (d:ds)

newtype Path = P [Direction] deriving (Eq, Show)

instance Read Path where
    readPrec = P <$> pDirections

input :: IO [Path]
input = map read . filter (not . null) . lines <$> readFile "input"

findCoord :: Path -> (Int, Int)
findCoord (P ds) = foldl go (0, 0) ds
    where go (x, y) E  = (x + 2, y)
          go (x, y) SE = (x + 1, y - 1)
          go (x, y) SW = (x - 1, y - 1)
          go (x, y) W  = (x - 2, y)
          go (x, y) NW = (x - 1, y + 1)
          go (x, y) NE = (x + 1, y + 1)

turnTiles :: [Path] -> S.Set (Int, Int)
turnTiles = foldl go S.empty
    where go ts p = if c `S.member` ts
                       then S.delete c ts
                       else S.insert c ts
             where c = findCoord p

part1solution :: IO ()
part1solution = print . S.size . turnTiles =<< input

coordsAround :: (Int, Int) -> [(Int, Int)]
coordsAround (x, y) = [(x + 2, y), (x + 1, y - 1), (x - 1, y - 1), (x -2, y), (x - 1, y + 1), (x + 1, y + 1)]

blacksAround :: S.Set (Int, Int) -> (Int, Int) -> Int
blacksAround ts = length . filter (==True) . map (`S.member` ts) . coordsAround

day :: S.Set (Int, Int) -> S.Set (Int, Int)
day ts = S.filter (\t -> let n = blacksAround ts t in n > 0 && n <= 2) ts
       <> S.filter (\t -> blacksAround ts t == 2) front
    where front = S.fromList . concatMap (filter (`S.notMember` ts) . coordsAround) . S.toList $ ts

part2solution :: IO ()
part2solution = do
    ts <- turnTiles <$> input
    print . S.size . foldr (const day) ts $ [1..100]
