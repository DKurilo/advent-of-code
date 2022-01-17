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

hashStream :: B.ByteString -> [(B.ByteString, Int)]
hashStream cs = map (\n -> (encode . hash $ cs <> (pack . show $ n), n)) [0 ..]

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ = id
applyN 1 f = f
applyN n f = applyN (n - 1) f . f

stretchedHashStream :: B.ByteString -> [(B.ByteString, Int)]
stretchedHashStream cs = map (\n -> (applyN 2017 (encode . hash) $ cs <> (pack . show $ n), n)) [0 ..]

getTripled :: B.ByteString -> [Word8]
getTripled =
  reverse . (\(ws, (w, n)) -> if n >= 3 then w : ws else ws)
    . B.foldl
      ( \(ws, (w, n)) w' ->
          if w == w'
            then (ws, (w, n + 1))
            else if n >= 3 then (w : ws, (w', 1)) else (ws, (w', 1))
      )
      ([], (0, 0))

keyStream :: [(B.ByteString, Int)] -> [(B.ByteString, Int)]
keyStream (hn@(h, n) : hs)
  | (not . null) ws && (any (B.isInfixOf (B.replicate 5 (head ws)) . fst) . take 1000) hs = hn : keyStream hs
  | otherwise = keyStream hs
  where
    ws = getTripled h

part1Solution :: B.ByteString -> (B.ByteString, Int)
part1Solution = (!! 63) . keyStream . hashStream

part2Solution :: B.ByteString -> (B.ByteString, Int)
part2Solution = (!! 63) . keyStream . stretchedHashStream
