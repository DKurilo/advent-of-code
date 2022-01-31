module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', nub)
import qualified Data.Map as M
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Claim = Claim {cId :: Int, cLeft :: Int, cTop :: Int, cWidth :: Int, cHeight :: Int} deriving (Show)

instance Read Claim where
  readPrec = do
    lift . P.char $ '#'
    claimId <- readPrec
    lift . P.string $ " @ "
    claimLeft <- readPrec
    lift . P.char $ ','
    claimTop <- readPrec
    lift . P.string $ ": "
    claimWidth <- readPrec
    lift . P.char $ 'x'
    Claim claimId claimLeft claimTop claimWidth <$> readPrec

putOnFabric :: M.Map (Int, Int) [Int] -> Claim -> M.Map (Int, Int) [Int]
putOnFabric fabric claim =
  M.unionsWith (++) . (fabric :) $
    [ M.singleton (x, y) [cId claim]
      | x <- take (cWidth claim) [cLeft claim ..],
        y <- take (cHeight claim) [cTop claim ..]
    ]

part1Solution :: [Claim] -> Int
part1Solution = length . filter ((> 1) . length) . M.elems . foldl' putOnFabric M.empty

part2Solution :: [Claim] -> Int
part2Solution claims = head notOverlapped
  where
    fabric = M.elems . foldl' putOnFabric M.empty $ claims
    overlapped = nub . concat . filter ((> 1) . length) $ fabric
    notOverlapped = nub . concat . filter (all (`notElem` overlapped)) $ fabric
