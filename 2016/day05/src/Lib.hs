module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.ByteString.Lazy.Internal (packChars)
import Data.Digest.Pure.MD5 (md5, md5DigestBytes)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

hashes :: String -> [String]
hashes cs =
  map (drop 5) . filter ("00000" `isPrefixOf`)
    . map (show . md5 . packChars . (cs ++) . show)
    $ [0 ..]

part1Solution :: String -> String
part1Solution = map head . take 8 . hashes

part2Solution :: String -> String
part2Solution = doer "--------" . hashes
  where
    doer cs (h@(c1 : c2 : _) : hs)
      | '-' `notElem` cs = cs
      | isNothing mbn = doer cs hs
      | n >= length cs = doer cs hs
      | cs !! n /= '-' = doer cs hs
      | otherwise = doer (take n cs ++ (c2 : drop (n + 1) cs)) hs
      where
        mbn = readMaybe [c1]
        (Just n) = mbn
