module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Digest.Pure.MD5 (md5)
import Data.Int (Int64)
import Data.List (isPrefixOf)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

findNumber :: (String -> Bool) -> B.ByteString -> Int
findNumber p bs =
  snd
    . head
    . filter (p . fst)
    . map (\n -> (show . md5 . (bs <>) . BC.pack . show $ n, n))
    $ [0 ..]

part1Solution :: B.ByteString -> Int
part1Solution = findNumber ("00000" `isPrefixOf`)

part2Solution :: B.ByteString -> Int
part2Solution = findNumber ("000000" `isPrefixOf`)
