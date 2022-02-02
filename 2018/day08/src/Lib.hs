module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Tree = Tree {tNodeCount :: Int, tNodes :: [Tree], tMetaCount :: Int, tMetas :: [Int]} deriving (Show)

parseTrees :: Int -> [Int] -> ([Tree], [Int])
parseTrees 0 xs = ([], xs)
parseTrees n xs = (tree : trees, xs'')
  where
    (tree, xs') = parseTree xs
    (trees, xs'') = parseTrees (n - 1) xs'

parseTree :: [Int] -> (Tree, [Int])
parseTree (cntN : cntM : xs) = (Tree cntN trees cntM meta, drop cntM xs')
  where
    (trees, xs') = parseTrees cntN xs
    meta = take cntM xs'

traverseTree :: (Tree -> [Int]) -> Tree -> [Int]
traverseTree f t = f t ++ (concatMap (traverseTree f) . tNodes) t

part1Solution :: [Int] -> Int
part1Solution = sum . traverseTree tMetas . fst . parseTree

rootValue :: Tree -> Int
rootValue t
  | tNodeCount t == 0 = sum . tMetas $ t
  | otherwise = sum . map nodeValue . tMetas $ t
  where
    nodeValue x
      | x == 0 = 0
      | x > tNodeCount t = 0
      | otherwise = rootValue (tNodes t !! (x - 1))

part2Solution :: [Int] -> Int
part2Solution = rootValue . fst . parseTree
