module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (mapM)
import Data.Char (isAlpha)
import Data.List (find)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Line = Line {lnName :: String, lnWeight :: Int, lnTops :: [String]} deriving (Eq, Show)

instance Read Line where
  readPrec = do
    name <- lift . P.munch1 $ isAlpha
    lift P.skipSpaces
    lift . P.char $ '('
    weight <- readPrec
    lift . P.char $ ')'
    lift P.skipSpaces
    fmap (Line name weight . concat) . lift . P.many $ do
      P.string "->"
      P.skipSpaces
      P.many $ do
        P.many $ do
          P.skipSpaces
          P.char ','
          P.skipSpaces
        P.munch1 isAlpha

findBottom :: Line -> [Line] -> Maybe Line
findBottom _ [] = Nothing
findBottom l (l' : ls)
  | lnName l `elem` lnTops l' = Just l'
  | otherwise = findBottom l ls

getRoot :: [Line] -> Maybe Line
getRoot [] = Nothing
getRoot (l : ls) = case l `findBottom` ls of
  Just l' -> getRoot (l' : filter (/= l') ls)
  _ -> Just l

findByName :: String -> [Line] -> Maybe Line
findByName n = find ((== n) . lnName)

data Tower = Tower {twName :: String, twWeight :: Int, twHolds :: Int, twTops :: [Tower]} deriving (Show)

weight :: Tower -> Int
weight t = twWeight t + (sum . map (\t' -> twWeight t' + twHolds t') . twTops) t

mkTower :: [Line] -> Maybe Tower
mkTower ls = do
  r <- getRoot ls
  let ls' = filter (/= r) ls
  tops <- mapM (`findByName` ls) . lnTops $ r
  subTowers <- mapM (\l -> mkTower (l : ls')) tops
  return $ Tower (lnName r) (lnWeight r) (sum . map weight $ subTowers) subTowers

findOneNotLikeOthers :: (Eq a) => [a] -> Maybe (a, a)
findOneNotLikeOthers xs
  | (length . take 4) xs < 3 = Nothing
  | x1 /= x2 && x2 == x3 = Just (x1, x2)
  | x2 /= x1 && x1 == x3 = Just (x2, x1)
  | x3 /= x1 && x1 == x2 = Just (x3, x1)
  | x1 /= x2 && x2 /= x3 = Nothing
  | otherwise = findOneNotLikeOthers . drop 1 $ xs
  where
    (x1 : x2 : x3 : _) = xs

findGoodWeightForUnbalanced :: Tower -> Maybe Int
findGoodWeightForUnbalanced t = case (findOneNotLikeOthers . map (\t' -> twWeight t' + twHolds t') . twTops) t of
  Just (wWrong, wGood) -> doer wWrong wGood
  _ -> Nothing
  where
    doer :: Int -> Int -> Maybe Int
    doer wWrong wGood = case (find (\t' -> twWeight t' + twHolds t' == wWrong) . twTops) t of
      Just t' -> case (findOneNotLikeOthers . map (\t'' -> twHolds t'' + twWeight t'') . twTops) t' of
        Just _ -> findGoodWeightForUnbalanced t'
        Nothing -> Just (wGood - twHolds t')
      _ -> Nothing

part1Solution :: [Line] -> Maybe String
part1Solution = fmap lnName . getRoot

part2Solution :: [Line] -> Maybe Int
part2Solution ls = findGoodWeightForUnbalanced =<< mkTower ls
