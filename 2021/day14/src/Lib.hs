module Lib
  ( part1Solution,
    part2Solution,
    Rules (..),
  )
where

import Data.Bifunctor (first)
import Data.Map (Map (..), elems, empty, foldrWithKey, fromList, fromListWith, insertWith, toList)
import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Input = Input Char (Map (Char, Char) Int) deriving (Show)

instance Read Input where
  readPrec = lift $ do
    cs <- P.many P.get
    return $ Input (head cs) (fromList . zipWith (\c1 c2 -> ((c1, c2), 1)) cs . tail $ cs)

inputToFreq :: Input -> Map Char Int
inputToFreq (Input c m) = insertWith (+) c 1 . fromListWith (+) . map (first snd) . toList $ m

newtype Rules = Rules (Map (Char, Char) Char) deriving (Show)

instance Read Rules where
  readPrec =
    Rules . fromList
      <$> ( lift
              . P.many
              $ do
                c1 <- P.get
                c2 <- P.get
                P.string " -> "
                c3 <- P.get
                P.char '\n'
                return ((c1, c2), c3)
          )

results :: Rules -> Input -> [Input]
results rules@(Rules rs) input@(Input c m) = input : results rules input'
  where
    input' = Input c . foldrWithKey insertNPairs empty $ m
    insertNPairs :: (Char, Char) -> Int -> Map (Char, Char) Int -> Map (Char, Char) Int
    insertNPairs cp@(c1, c2) n m = case cp `M.lookup` rs of
      Just c -> insertWith (+) (c1, c) n . insertWith (+) (c, c2) n $ m
      _ -> m

findDiffOnStep :: Int -> Rules -> Input -> Int
findDiffOnStep n rs input = maxC - minC
  where
    fr = inputToFreq (results rs input !! n)
    maxC = maximum fr
    minC = minimum fr

part1Solution :: Rules -> Input -> Int
part1Solution = findDiffOnStep 10

part2Solution :: Rules -> Input -> Int
part2Solution = findDiffOnStep 40
