module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (isInfixOf, isSuffixOf)
import Data.List.Split (splitOn)
import Data.Sequence hiding (filter, length)
import qualified Data.Sequence as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

moveElf :: S.Seq Int -> Int -> Int -> (Int, Int)
moveElf sb ep en = (ep', sb `index` ep')
  where
    ep' = (ep + en + 1) `mod` S.length sb

nextSb :: (S.Seq Int, ((Int, Int), (Int, Int))) -> (S.Seq Int, ((Int, Int), (Int, Int)))
nextSb (sb, ((ep1, en1), (ep2, en2)))
  | s < 10 = (sb', ((ep1', en1'), (ep2', en2')))
  | otherwise = (sb'', ((ep1'', en1''), (ep2'', en2'')))
  where
    s = en1 + en2
    n1 = s `mod` 10
    n2 = s `div` 10
    sb' = sb |> n1
    sb'' = sb |> n2 |> n1
    (ep1', en1') = moveElf sb' ep1 en1
    (ep2', en2') = moveElf sb' ep2 en2
    (ep1'', en1'') = moveElf sb'' ep1 en1
    (ep2'', en2'') = moveElf sb'' ep2 en2

scoreboard :: Int -> (S.Seq Int, ((Int, Int), (Int, Int)))
scoreboard l = head . filter ((> l) . S.length . fst) . iterate nextSb $ (S.fromList [3, 7], ((0, 3), (1, 7)))

part1Solution :: Int -> String
part1Solution n = concatMap show . S.take 10 . S.drop n . fst . scoreboard $ (n + 10)

canBeStartIndex :: Eq a => [a] -> [a] -> Int
canBeStartIndex [] hs = length hs
canBeStartIndex ns hs
  | ns `isSuffixOf` hs = length hs - length ns
  | otherwise = canBeStartIndex (init ns) hs

part2Solution :: String -> Int
part2Solution cs = doer 0 initSb
  where
    initSb = scoreboard . length $ cs
    doer n sb
      | cs `isInfixOf` cs' = n + (length . (!! 0) . splitOn cs) cs'
      | otherwise = doer (n + canBeStartIndex cs cs') . nextSb $ sb
      where
        cs' = concatMap show . S.drop n . fst $ sb
