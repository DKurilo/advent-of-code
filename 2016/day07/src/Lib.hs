module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Data.List (intersect)
import Data.Tuple (swap)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data IPv7Part = Supernet String | Hypernet String

instance Read IPv7Part where
  readPrec =
    ( do
        lift . P.char $ '['
        cs <- lift (P.munch1 isAlpha)
        lift . P.char $ ']'
        return $ Hypernet cs
    )
      PC.<++ (Supernet <$> lift (P.munch1 isAlpha))

isHypernet :: IPv7Part -> Bool
isHypernet (Hypernet _) = True
isHypernet _ = False

ipv7partValue :: IPv7Part -> String
ipv7partValue (Supernet cs) = cs
ipv7partValue (Hypernet cs) = cs

newtype IPv7 = IPv7 [IPv7Part]

instance Read IPv7 where
  readPrec = IPv7 <$> (lift . P.many1) (PC.readPrec_to_P readPrec 0)

parts :: IPv7 -> [IPv7Part]
parts (IPv7 ps) = ps

hasPair :: String -> Bool
hasPair cs
  | length cs < 4 = False
  | c1 /= c2 && c1 == c4 && c2 == c3 = True
  | otherwise = hasPair . tail $ cs
  where
    (c1 : c2 : c3 : c4 : _) = cs

hasABBA :: IPv7Part -> Bool
hasABBA = hasPair . ipv7partValue

supportsTLS :: IPv7 -> Bool
supportsTLS addr = (not . any hasABBA . filter isHypernet . parts) addr && (any hasABBA . filter (not . isHypernet) . parts) addr

part1Solution :: [IPv7] -> Int
part1Solution = length . filter supportsTLS

abas :: String -> [(Char, Char)]
abas cs
  | length cs < 3 = []
  | c1 /= c2 && c1 == c3 = (c1, c2) : (abas . tail) cs
  | otherwise = abas . tail $ cs
  where
    (c1 : c2 : c3 : _) = cs

supportsSSL :: IPv7 -> Bool
supportsSSL addr =
  not . null $
    intersect
      (concatMap (abas . ipv7partValue) . filter (not . isHypernet) . parts $ addr)
      (map swap . concatMap (abas . ipv7partValue) . filter isHypernet . parts $ addr)

part2Solution :: [IPv7] -> Int
part2Solution = length . filter supportsSSL
