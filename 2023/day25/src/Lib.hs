module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlphaNum)
import Data.List (foldl', intercalate, nub, sortOn)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

newtype Graph = Graph {ge :: M.Map String [String]} deriving (Show)

instance Read Graph where
  readPrec = lift $ do
    xs <- P.many $ do
      cs <- P.munch isAlphaNum
      _ <- P.string ": "
      css <- P.many $ do
        cs' <- P.munch isAlphaNum
        _ <- P.char '\n' P.<++ P.char ' '
        return cs'
      return (cs, css)
    let es =
          foldl'
            ( \m (cs, css) ->
                foldl'
                  ( \m' cs' ->
                      M.insertWith (++) cs' [cs] m'
                  )
                  (M.insertWith (++) cs css m)
                  css
            )
            M.empty
            xs

    return $ Graph es

area :: Graph -> String -> Int
area g n = length . nub . doer [n] $ [n]
  where
    doer :: [String] -> [String] -> [String]
    doer front visited
      | null front = visited
      | otherwise = doer front' visited'
      where
        front' = concatMap (filter (`notElem` visited) . (ge g M.!)) front
        visited' = front' <> visited

distance :: Graph -> String -> String -> Int
distance g n finish = doer [n] [n] 0
  where
    doer :: [String] -> [String] -> Int -> Int
    doer front visited steps
      | finish `elem` front = steps
      | null front = error "Something is wrong"
      | otherwise = doer front' visited' (steps + 1)
      where
        front' = concatMap (filter (`notElem` visited) . (ge g M.!)) front
        visited' = front' <> visited

removeEdge :: String -> String -> Graph -> Graph
removeEdge n1 n2 g = g {ge = M.adjust (filter (/= n1)) n2 . M.adjust (filter (/= n2)) n1 . ge $ g}

part1Solution :: String -> Int
part1Solution css = area unconnectedGraph startN1 * area unconnectedGraph startN2
  where
    graph = read css
    edgesToRemove =
      fmap fst
        . take 3
        . sortOn (negate . snd)
        . fmap (\(i, j) -> ((i, j), distance (removeEdge i j graph) i j))
        $ [(i, j) | i <- (M.keys . ge) graph, j <- ge graph M.! i, i >= j]
    unconnectedGraph = foldl' (\g (n1, n2) -> removeEdge n1 n2 g) graph edgesToRemove
    (startN1, startN2) = head edgesToRemove

part2Solution :: String -> Int
part2Solution = length
