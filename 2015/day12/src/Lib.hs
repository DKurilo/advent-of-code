{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Aeson (Value (..), decode)
import Data.Aeson.KeyMap (KeyMap, elems)
import qualified Data.ByteString.Lazy as BS
import Data.Maybe (fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text (..))
import qualified Data.Vector as V
import Debug.Trace (trace)

allNumbers :: (KeyMap Value -> Bool) -> Value -> [Int]
allNumbers p (Object o) = if p o then (concatMap (allNumbers p) . elems) o else [0]
allNumbers p (Array a) = concatMap (allNumbers p) a
allNumbers _ (String s) = [0]
allNumbers _ (Number n) = [fromMaybe 0 . toBoundedInteger $ n]
allNumbers _ (Bool b) = [0]
allNumbers _ Null = [0]

hasValue :: Value -> KeyMap Value -> Bool
hasValue v = elem v . elems

part1Solution :: BS.ByteString -> Int
part1Solution = maybe 0 (sum . allNumbers (const True)) . decode

part2Solution :: BS.ByteString -> Int
part2Solution = maybe 0 (sum . allNumbers (not . hasValue (String "red"))) . decode
