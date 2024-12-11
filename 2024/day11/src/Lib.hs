{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution
  , part2Solution
  ) where

import Control.Applicative ((<|>))

-- import Control.Monad (join)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

rule1 :: String -> Maybe [String]
rule1 "0" = Just ["1"]
rule1 _ = Nothing

rule2 :: String -> Maybe [String]
rule2 cs
  | even lcs =
    Just [take lcs2 cs, show . (read :: (String -> Integer)) . drop lcs2 $ cs]
  | otherwise = Nothing
  where
    lcs = length cs
    lcs2 = lcs `div` 2

rule3 :: String -> Maybe [String]
rule3 = Just . (: []) . show . (* (2024 :: Integer)) . read

applyRules :: String -> Maybe [String]
applyRules n = rule1 n <|> rule2 n <|> rule3 n

-- blinkMany :: Int -> [String] -> Int
-- blinkMany n =
--   maybe 0 length
--     . (!! n)
--     . iterate (\mbNs -> (fmap join . mapM applyRules) =<< mbNs)
--     . Just
blink :: M.Map String Int -> M.Map String Int
blink =
  M.fromListWith (+)
    . concatMap (\(x, n) -> fmap (, n) . fromMaybe [] . applyRules $ x)
    . M.toList

blinkMany :: Int -> [String] -> Int
blinkMany n = sum . M.elems . (!! n) . iterate blink . M.fromList . fmap (, 1)

part1Solution :: [String] -> Int
part1Solution = blinkMany 25

part2Solution :: [String] -> Int
part2Solution = blinkMany 75
