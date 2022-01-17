{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Crypto.Hash.MD5 (hash)
import qualified Data.ByteString as B
import Data.ByteString.Base16 (encode)
import Data.ByteString.Char8 (pack)
import Data.Word (Word8 (..))
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

possibleWays :: B.ByteString -> Int -> Int -> (Int, Int) -> B.ByteString -> [((Int, Int), B.ByteString)]
possibleWays passcode w h (x, y) path =
  [((x, y - 1), path <> "U") | isOpenUp, y > 0]
    ++ [((x, y + 1), path <> "D") | isOpenDown, y < (h - 1)]
    ++ [((x - 1, y), path <> "L") | isOpenLeft, x > 0]
    ++ [((x + 1, y), path <> "R") | isOpenRight, x < (w - 1)]
  where
    hs = encode . hash $ passcode <> path
    isOpenUp = isOpen . B.take 1 $ hs
    isOpenDown = isOpen . B.take 1 . B.drop 1 $ hs
    isOpenLeft = isOpen . B.take 1 . B.drop 2 $ hs
    isOpenRight = isOpen . B.take 1 . B.drop 3 $ hs
    isOpen = (`B.isInfixOf` "bcdef")

findPathShortest :: B.ByteString -> Int -> Int -> (Int, Int) -> (Int, Int) -> [B.ByteString]
findPathShortest passcode w h start end = doer [(start, "")]
  where
    doer cps
      | null cps = []
      | (not . null) found = map snd found
      | otherwise = doer . concatMap (uncurry (possibleWays passcode w h)) $ cps
      where
        found = filter ((== end) . fst) cps

part1Solution :: B.ByteString -> [B.ByteString]
part1Solution passcode = findPathShortest passcode 4 4 (0, 0) (3, 3)

findPathLongest :: B.ByteString -> Int -> Int -> (Int, Int) -> (Int, Int) -> Int
findPathLongest passcode w h start end = doer [(start, "")] 0
  where
    doer cps longestSoFar
      | null cps = longestSoFar
      | (not . null) found = doer (filter ((/= end) . fst) cps) (B.length . head . map snd $ found)
      | otherwise = doer (concatMap (uncurry (possibleWays passcode w h)) cps) longestSoFar
      where
        found = filter ((== end) . fst) cps

part2Solution :: B.ByteString -> Int
part2Solution passcode = findPathLongest passcode 4 4 (0, 0) (3, 3)
