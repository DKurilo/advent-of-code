{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Hashable (Hashable (..))
import Data.List (isPrefixOf)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import Text.AhoCorasick
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

getAllSubst :: forall a. (Ord a, Hashable a) => StateMachine a [a] -> M.Map [a] [[a]] -> [a] -> S.Set [a]
getAllSubst m rules mol =
  S.fromList
    . concatMap
      ( \(Position i l from) ->
          let (pref, suf') = splitAt i mol
              suf = drop l suf'
           in case from `M.lookup` rules of
                Just tos -> map (\to -> pref ++ to ++ suf) tos
                _ -> []
      )
    . findAll m
    $ mol

part1Solution :: (Ord a, Hashable a) => M.Map [a] [[a]] -> [a] -> Int
part1Solution rules = S.size . getAllSubst stm rules
  where
    stm = makeSimpleStateMachine . M.keys $ rules

findShortestPath :: forall a. (Ord a, Hashable a, Show a) => StateMachine a [a] -> S.Set [a] -> M.Map [a] [[a]] -> S.Set [a] -> Maybe Int
findShortestPath m mols rules fins
  | (not . S.null) (mols `S.intersection` fins) = Just 1
  | otherwise = (1 +) <$> findShortestPath m mols' rules fins
  where
    mols' = S.fromList . concatMap applyRuleToStart . S.toList $ mols
    applyRuleToStart :: [a] -> [[a]]
    applyRuleToStart xs =
      concatMap
        ( \(Position i l from) ->
            let suf = drop l suf'
             in case from `M.lookup` rules of
                  Just tos -> map (\to -> pref ++ to ++ suf) tos
                  _ -> []
        )
        poss'
      where
        poss = findAll m xs
        minPos = minimum . map (\(Position i _ _) -> i) $ poss
        poss' = filter (\(Position i _ _) -> i == minPos) poss
        (pref, suf') = splitAt minPos xs

inverseRules :: Ord a => M.Map [a] [[a]] -> M.Map [a] [[a]]
inverseRules = M.fromListWith (++) . concatMap (\(from, tos) -> map ((,[reverse from]) . reverse) tos) . M.toList

-- So we are starting from the end of the string and applying only last possible substitution
-- There is a trick that prevents blinking strings. Like HOHOHO -> HOHOHe -> HOHOHH -> HOHOHe -> ...
-- In the same time if rules contains blinking substitution that will be at the end of the string
-- we still will have a problem
-- I didn't figure out proper solution for this things
part2Solution :: M.Map String [String] -> String -> Maybe Int
part2Solution rules mol = findShortestPath stm (S.singleton . reverse $ mol) rules' fins
  where
    rules' = inverseRules . M.delete "e" $ rules
    fins = S.fromList . maybe [] (map reverse) . M.lookup "e" $ rules
    stm = makeSimpleStateMachine . M.keys $ rules'
