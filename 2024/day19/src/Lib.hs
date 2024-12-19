module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M

-- white | blue | black | red | green
data Stripe
  = W
  | U
  | B
  | R
  | G
  deriving (Eq, Ord, Show)

charToStripe :: Char -> Stripe
charToStripe 'w' = W
charToStripe 'u' = U
charToStripe 'b' = B
charToStripe 'r' = R
charToStripe _ = G

data Ord a =>
     Trie a = Trie
  { tIsEnd :: Bool
  , tSuff :: M.Map a (Trie a)
  } deriving (Show, Eq, Ord)

emptyTrie :: (Ord a) => Trie a
emptyTrie = Trie False M.empty

addToTrie :: (Ord a) => [a] -> Trie a -> Trie a
addToTrie [] t = t {tIsEnd = True}
addToTrie (x:xs) t = Trie (tIsEnd t) suff
  where
    suff =
      case x `M.lookup` tSuff t of
        Just t' -> M.insert x (addToTrie xs t') (tSuff t)
        Nothing -> M.insert x (addToTrie xs emptyTrie) (tSuff t)

type Design = [Stripe]

parse :: String -> (Trie Stripe, [Design])
parse ls = (tr, fmap (fmap charToStripe) . lines . last $ sepLs)
  where
    sepLs = splitOn "\n\n" ls
    ts = fmap (fmap charToStripe) . splitOn ", " . head $ sepLs
    tr = foldl' (flip addToTrie) emptyTrie ts

waysToBuild :: Design -> Trie Stripe -> Int
waysToBuild d t = fst . doer d t $ M.empty
  where
    doer ::
         Design
      -> Trie Stripe
      -> M.Map (Design, Trie Stripe) Int
      -> (Int, M.Map (Design, Trie Stripe) Int)
    doer [] t' memo =
      ( if tIsEnd t'
          then 1
          else 0
      , memo)
    doer d'@(x:xs) t' memo =
      case (d', t') `M.lookup` memo of
        Just res -> (res, memo)
        Nothing ->
          case x `M.lookup` tSuff t' of
            Just t''
              | (not . tIsEnd) t'' -> (r', M.insert (d', t') r' memo')
              | otherwise -> (r' + r'', M.insert (d', t') (r' + r'') memo'')
              where (r', memo') = doer xs t'' memo
                    (r'', memo'') = doer xs t memo'
            _ -> (0, M.insert (d', t') 0 memo)

part1Solution :: String -> Int
part1Solution cs = length . filter ((> 0) . (`waysToBuild` ts)) $ ds
  where
    (ts, ds) = parse cs

part2Solution :: String -> Int
part2Solution cs = sum . fmap (`waysToBuild` ts) $ ds
  where
    (ts, ds) = parse cs
