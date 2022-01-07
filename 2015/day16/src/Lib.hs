module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Sue = Sue Int (M.Map String Int) deriving (Show)

sue :: Sue -> Int
sue (Sue n _) = n

readTrait :: P.ReadP (String, Int)
readTrait = do
  P.skipSpaces
  tr <- P.munch1 isAlpha
  P.skipSpaces
  P.char ':'
  P.skipSpaces
  n <- PC.readPrec_to_P readPrec 0
  P.skipMany . P.char $ ','
  return (tr, n)

instance Read Sue where
  readPrec = do
    (lift . P.string) "Sue "
    n <- readPrec
    lift P.skipSpaces
    (lift . P.char) ':'
    trs <- lift . P.many1 $ readTrait
    return . Sue n . M.fromList $ trs

hasTrait :: String -> (Int -> Bool) -> Sue -> Maybe Bool
hasTrait cs p (Sue _ ps) = p <$> cs `M.lookup` ps

filterByTrait :: String -> (Int -> Bool) -> [Sue] -> [Sue]
filterByTrait tr p =
  filter
    ( \s -> case hasTrait tr p s of
        Just b -> b
        _ -> True
    )

part1Solution :: [Sue] -> Int
part1Solution =
  sue
    . head
    . filterByTrait "perfumes" (== 1)
    . filterByTrait "cars" (== 2)
    . filterByTrait "trees" (== 3)
    . filterByTrait "goldfish" (== 5)
    . filterByTrait "vizslas" (== 0)
    . filterByTrait "akitas" (== 0)
    . filterByTrait "pomeranians" (== 3)
    . filterByTrait "samoyeds" (== 2)
    . filterByTrait "cats" (== 7)
    . filterByTrait "children" (== 3)

part2Solution :: [Sue] -> Int
part2Solution =
  sue
    . head
    . filterByTrait "perfumes" (== 1)
    . filterByTrait "cars" (== 2)
    . filterByTrait "trees" (> 3)
    . filterByTrait "goldfish" (< 5)
    . filterByTrait "vizslas" (== 0)
    . filterByTrait "akitas" (== 0)
    . filterByTrait "pomeranians" (< 3)
    . filterByTrait "samoyeds" (== 2)
    . filterByTrait "cats" (> 7)
    . filterByTrait "children" (== 3)
