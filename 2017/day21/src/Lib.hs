{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Guidebook = Gb {unGb :: M.Map [[Bool]] [[Bool]]} deriving (Show)

parseMatrix :: P.ReadP [[Bool]]
parseMatrix = fmap (map (map (== '#')) . splitOn "/") . P.munch1 $ (\c -> c == '.' || c == '#' || c == '/')

instance Read Guidebook where
  readPrec = fmap (Gb . M.fromList . concatMap (\(from, to) -> map (,to) . allVariants $ from)) . lift . P.many $ do
    from <- parseMatrix
    P.string " => "
    to <- parseMatrix
    P.skipSpaces
    return (from, to)

allVariants :: [[a]] -> [[[a]]]
allVariants mx = allRotations mx ++ (allRotations . flipMx) mx
  where
    allRotations = take 4 . iterate rotateMx

flipMx :: [[a]] -> [[a]]
flipMx = map reverse

rotateMx :: [[a]] -> [[a]]
rotateMx mx = [[mx !! i !! (size - j) | i <- [0 .. size]] | j <- [0 .. size]]
  where
    size = length mx - 1

breakOnSquares :: Int -> [[a]] -> [[[[a]]]]
breakOnSquares n mx =
  [ [ [[mx !! j' !! i' | i' <- [(n * i) .. (n * (i + 1) - 1)]] | j' <- [(n * j) .. (n * (j + 1) - 1)]]
      | i <- [0 .. l' - 1]
    ]
    | j <- [0 .. l' - 1]
  ]
  where
    l' = length mx `div` n

joinSquares :: [[[[a]]]] -> [[a]]
joinSquares mx = [[mx !! (j `div` n) !! (i `div` n) !! (j `mod` n) !! (i `mod` n) | i <- [0 .. l - 1]] | j <- [0 .. l - 1]]
  where
    n = length . head . head $ mx
    l = length mx * n

default2Rule = [[False, False], [False, False]]

default3Rule = [[False, False, False], [False, False, False], [False, False, False]]

applyRules :: Guidebook -> [[Bool]] -> [[Bool]]
applyRules gb mx = case mx `M.lookup` unGb gb of
  Just mx' -> mx'
  _
    | length mx == 2 -> default2Rule
    | otherwise -> default3Rule

transform :: Guidebook -> [[Bool]] -> [[Bool]]
transform gb mx
  | even l = doer 2
  | l `mod` 3 == 0 = doer 3
  | otherwise = error "Something is totally wrong"
  where
    doer n = joinSquares . map (map (applyRules gb)) . breakOnSquares n $ mx
    l = length mx

initMx :: [[Bool]]
initMx = [[False, True, False], [False, False, True], [True, True, True]]

onAfter :: Int -> Guidebook -> Int
onAfter n gb = length . filter id . concat . (!! n) . iterate (transform gb) $ initMx

part1Solution :: Guidebook -> Int
part1Solution = onAfter 5

part2Solution :: Guidebook -> Int
part2Solution = onAfter 18
