module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', minimumBy, nub, sort, sortBy)
import qualified Data.Map as M
import Data.Ord (Down (..), comparing)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Card = CJ2 | C2 | C3 | C4 | C5 | C6 | C7 | C8 | C9 | CT | CJ | CQ | CK | CA deriving (Eq, Ord, Show)

instance Read Card where
  readPrec =
    lift $
      ( do
          _ <- P.char '2'
          return C2
      )
        P.<++ ( do
                  _ <- P.char '3'
                  return C3
              )
        P.<++ ( do
                  _ <- P.char '4'
                  return C4
              )
        P.<++ ( do
                  _ <- P.char '5'
                  return C5
              )
        P.<++ ( do
                  _ <- P.char '6'
                  return C6
              )
        P.<++ ( do
                  _ <- P.char '7'
                  return C7
              )
        P.<++ ( do
                  _ <- P.char '8'
                  return C8
              )
        P.<++ ( do
                  _ <- P.char '9'
                  return C9
              )
        P.<++ ( do
                  _ <- P.char 'T'
                  return CT
              )
        P.<++ ( do
                  _ <- P.char 'J'
                  return CJ
              )
        P.<++ ( do
                  _ <- P.char 'Q'
                  return CQ
              )
        P.<++ ( do
                  _ <- P.char 'K'
                  return CK
              )
        P.<++ ( do
                  _ <- P.char 'A'
                  return CA
              )

data Rank = RHigh | R1 | R2 | R3 | RFull | R4 | R5 deriving (Eq, Ord, Show)

cardsRank :: [Card] -> Rank
cardsRank cs
  | M.size mcs == 1 = R5
  | M.size mcs == 2 && head ns == 4 = R4
  | M.size mcs == 2 && head ns == 3 = RFull
  | M.size mcs == 3 && head ns == 3 = R3
  | M.size mcs == 3 && head ns == 2 && ns !! 1 == 2 = R2
  | M.size mcs == 4 = R1
  | otherwise = RHigh
  where
    mcs :: M.Map Card Int
    mcs = foldl' (\mcs' c -> M.insertWith (+) c 1 mcs') M.empty cs
    ns = (sortBy (comparing Down) . M.elems) mcs

data Hand = Hand {hCards :: [Card], hRank :: Rank, hBid :: Int} deriving (Show)

instance Read Hand where
  readPrec = lift $ do
    cs <- P.many (readPrec_to_P readPrec 0)
    P.skipSpaces
    bid <- readPrec_to_P readPrec 0
    return (Hand cs (cardsRank cs) bid)

instance Eq Hand where
  h1 == h2 = hRank h1 == hRank h2 && hCards h1 == hCards h2

instance Ord Hand where
  h1 <= h2 = hRank h1 < hRank h2 || (hRank h1 == hRank h2 && hCards h1 <= hCards h2)

part1Solution :: [String] -> Int
part1Solution = sum . zipWith (\n c -> n * hBid c) [1 ..] . sort . fmap read

cardsRank2 :: [Card] -> Rank
cardsRank2 cs
  | CJ2 `notElem` cs = cardsRank cs
  | otherwise =
      ( minimumBy (comparing Down)
          . fmap (\c -> cardsRank . fmap (\c' -> if c' == CJ2 then c else c') $ cs)
          . nub
          . filter (/= CJ2)
          . (CA :)
      )
        cs

handToHand2 :: Hand -> Hand
handToHand2 h = h {hCards = cs, hRank = rank}
  where
    cs = fmap (\c -> if c == CJ then CJ2 else c) . hCards $ h
    rank = cardsRank2 cs

part2Solution :: [String] -> Int
part2Solution = sum . zipWith (\n c -> n * hBid c) [1 ..] . sort . fmap (handToHand2 . read)
