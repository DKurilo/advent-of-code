{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

parseGrove :: [String] -> S.Set Point
parseGrove =
  S.fromList
    . map fst
    . filter ((== '#') . snd)
    . concat
    . zipWith (\j -> zipWith (\i c -> (Point i j, c)) [0 ..]) [0 ..]

groveRound :: (S.Set Point, (Int, Bool)) -> (S.Set Point, (Int, Bool))
groveRound (elves, (n, _)) = (elves', (n + 1, (not . null) decisions))
  where
    decisions =
      M.fromListWith (<>)
        . concatMap
          ( \p ->
              [ (head availableDirections, [p])
                | let ds = directions p,
                  let availableDirections = [p' | (p', ps) <- ds, all (`S.notMember` elves) ps],
                  length availableDirections < 4 && (not . null) availableDirections
              ]
          )
        . S.toList
        $ elves
    directions p =
      take 4 . drop (n `mod` 4) . cycle $
        [ (p {pY = pY p - 1}, [p {pY = pY p - 1, pX = pX p + i} | i <- [-1 .. 1]]),
          (p {pY = pY p + 1}, [p {pY = pY p + 1, pX = pX p + i} | i <- [-1 .. 1]]),
          (p {pX = pX p - 1}, [p {pY = pY p + i, pX = pX p - 1} | i <- [-1 .. 1]]),
          (p {pX = pX p + 1}, [p {pY = pY p + i, pX = pX p + 1} | i <- [-1 .. 1]])
        ]
    elves' =
      foldl' (\elves'' (toP, fromPs) -> S.insert toP . S.delete (head fromPs) $ elves'') elves
        . M.toList
        . M.filter ((== 1) . length)
        $ decisions

showGrove :: S.Set Point -> String
showGrove elves =
  intercalate
    "\n"
    [ [ if Point x y `S.member` elves
          then '#'
          else '.'
        | x <- [minX .. maxX]
      ]
      | y <- [minY .. maxY]
    ]
  where
    xs = S.map pX elves
    minX = fromMaybe 0 . S.lookupMin $ xs
    maxX = fromMaybe 0 . S.lookupMax $ xs
    ys = S.map pY elves
    minY = fromMaybe 0 . S.lookupMin $ ys
    maxY = fromMaybe 0 . S.lookupMax $ ys

emptySpace :: S.Set Point -> Int
emptySpace elves = (maxX - minX + 1) * (maxY - minY + 1) - S.size elves
  where
    xs = S.map pX elves
    minX = fromMaybe 0 . S.lookupMin $ xs
    maxX = fromMaybe 0 . S.lookupMax $ xs
    ys = S.map pY elves
    minY = fromMaybe 0 . S.lookupMin $ ys
    maxY = fromMaybe 0 . S.lookupMax $ ys

part1Solution :: [String] -> Int
part1Solution = emptySpace . fst . (!! 10) . iterate groveRound . (,(0, True)) . parseGrove

part2Solution :: [String] -> Int
part2Solution = length . takeWhile (snd . snd) . iterate groveRound . (,(0, True)) . parseGrove
