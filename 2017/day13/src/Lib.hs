module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.Map as M
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Firewall = FW {unFW :: M.Map Int Int} deriving (Show)

instance Read Firewall where
  readPrec = fmap (FW . M.fromList) . lift . P.many $ do
    depth <- PC.readPrec_to_P readPrec 0
    P.skipSpaces
    P.char ':'
    P.skipSpaces
    range <- PC.readPrec_to_P readPrec 0
    P.skipSpaces
    return (depth, range)

severity :: (Int -> Int -> Int) -> Int -> Int -> Firewall -> Int
severity f start depth (FW fw) = case depth `M.lookup` fw of
  Just range
    | (start + depth) `mod` (2 * (range - 1)) == 0 -> f depth range
  _ -> 0

rideSeverity :: (Int -> Int -> Int) -> Int -> Firewall -> Int
rideSeverity f start fw = sum . map (\depth -> severity f start depth fw) $ [0 .. lastPicoSec]
  where
    lastPicoSec = maximum . M.keys . unFW $ fw

part1Solution :: Firewall -> Int
part1Solution = rideSeverity (*) 0

part2Solution :: Firewall -> Int
part2Solution fw = fst . head . filter ((== 0) . snd) . map (\start -> (start, rideSeverity (\_ _ -> 1) start fw)) $ [0 ..]
