module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import           Control.Monad                (forM)
import           Data.List                    (sort)
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

data BackFront = F | B deriving (Eq, Show, Ord)

instance Read BackFront where
    readPrec = lift $ do
       c <- P.char 'B' <|> P.char 'F'
       return $ case c of
                  'B' -> B
                  'F' -> F

data LeftRight = L | R deriving (Eq, Show, Ord)

instance Read LeftRight where
    readPrec = lift $ do
       c <- P.char 'L' <|> P.char 'R'
       return $ case c of
                  'L' -> L
                  'R' -> R

newtype RowPicker = RP [BackFront] deriving (Eq, Show, Ord)

instance Read RowPicker where
    readPrec = RP  <$> forM [0..6] (const readPrec)

newtype SeatPicker = SP [LeftRight] deriving (Eq, Show, Ord)

instance Read SeatPicker where
    readPrec = SP <$> forM [0..2] (const readPrec)

data BoardingPass = BP RowPicker SeatPicker deriving (Eq, Show, Ord)

instance Read BoardingPass where
    readPrec = do
        rp <- readPrec
        BP rp <$> readPrec

seatNumber :: BoardingPass -> Int
seatNumber (BP (RP bfs) (SP lrs)) = row * 8 + seat
    where row = foldl (\r bf -> r * 2 + if bf == F then 0 else 1) 0 bfs
          seat = foldl (\s lr -> s * 2 + if lr == L then 0 else 1) 0 lrs

input :: IO [BoardingPass]
input = map read . filter (not . null) . lines <$> readFile "./input"

part1solution :: IO ()
part1solution = print . seatNumber . maximum =<< input


part2solution :: IO ()
part2solution = do
    seats <- sort . map seatNumber <$> input
    print . head . foldl (\ss (s1, s2) -> if (s2 - s1) > 1 then (s1 + 1) : ss else ss) [] . zip seats $ tail seats
