module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', intersect)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Card = Card {cardId :: Int, cardWin :: [Int], cardNumbs :: [Int]} deriving (Show)

data Pile = Pile {pileCads :: M.Map Int Card, pileAmount :: M.Map Int Int} deriving (Show)

instance Read Card where
  readPrec = lift $ do
    _ <- P.string "Card"
    P.skipSpaces
    cid <- readPrec_to_P readPrec 0
    P.skipSpaces
    _ <- P.char ':'
    P.skipSpaces
    wins <- P.many $ do
      x <- readPrec_to_P readPrec 0
      P.skipSpaces
      return x
    _ <- P.char '|'
    P.skipSpaces
    nums <- P.many $ do
      x <- readPrec_to_P readPrec 0
      P.skipSpaces
      return x
    return $ Card cid wins nums

getWinningCount :: Card -> Int
getWinningCount c = length $ cardWin c `intersect` cardNumbs c

getPoints :: Card -> Int
getPoints c
  | winning == 0 = 0
  | otherwise = 2 ^ (winning - 1)
  where
    winning = getWinningCount c

part1Solution :: [String] -> Int
part1Solution = sum . fmap (getPoints . read)

mkPile :: [Card] -> Pile
mkPile cs =
  Pile
    { pileCads = M.fromList . fmap (\c -> (cardId c, c)) $ cs,
      pileAmount = M.fromList . fmap (\c -> (cardId c, 1)) $ cs
    }

processPile :: Pile -> Pile
processPile p = foldl' processCard p . M.elems . pileCads $ p

processCard :: Pile -> Card -> Pile
processCard p c
  | winning == 0 = p
  | otherwise =
      foldl'
        ( \p' n ->
            p' {pileAmount = M.adjust (+ amountC) (cardId c + n) . pileAmount $ p'}
        )
        p
        [1 .. winning]
  where
    winning = getWinningCount c
    amountC = fromMaybe 0 . M.lookup (cardId c) . pileAmount $ p

part2Solution :: [String] -> Int
part2Solution = sum . M.elems . pileAmount . processPile . mkPile . fmap read
