module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (delete, nub)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Part = Part {pc1 :: Int, pc2 :: Int} deriving (Eq, Show)

instance Read Part where
  readPrec = do
    p1 <- readPrec
    lift . P.char $ '/'
    p2 <- readPrec
    return $ Part (min p1 p2) (max p1 p2)

partStrength :: Part -> Int
partStrength p = pc1 p + pc2 p

canConnect :: Int -> Part -> Bool
canConnect n p = n == pc1 p || n == pc2 p

bridgeStrength :: [Part] -> Int
bridgeStrength = sum . map partStrength

-- Assuing that each part is unique
bridges :: [Part] -> [[Part]]
bridges = doer 0
  where
    doer :: Int -> [Part] -> [[Part]]
    doer n ps
      | null next = [[]]
      | otherwise = concatMap (\p -> map (p :) . doer (if pc1 p == n then pc2 p else pc1 p) . delete p $ ps) next
      where
        next = nub . filter (canConnect n) $ ps

part1Solution :: [Part] -> Int
part1Solution = maximum . map bridgeStrength . bridges

part2Solution :: [Part] -> Int
part2Solution ps = maximum . map bridgeStrength . filter ((== maxL) . length) $ bs
  where
    bs = bridges ps
    maxL = maximum . map length $ bs
