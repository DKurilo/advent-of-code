module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (elemIndex, foldl')
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

toNumb :: Char -> Int
toNumb c = fromMaybe 0 $ elemIndex c ['0'..'9']

strDigits :: [(String, Char)]
strDigits = zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] "123456789"

extractNumber :: String -> Int
extractNumber cs = dFirst * 10 + dLast
  where
    digits = filter isDigit cs
    digits'
      | null digits = "0"
      | otherwise = digits
    dFirst = toNumb . head $ digits'
    dLast = toNumb . last $ digits'

prepareString :: String -> String
prepareString "" = ""
prepareString cs
  | (isDigit . head) cs = head cs : rest
  | otherwise = concatMap (applyReplace cs) strDigits <> rest
  where
    applyReplace :: String -> (String, Char) -> String
    applyReplace cs' (ds, d)
      | take lds cs' == ds = [d]
      | otherwise = ""
      where lds = length ds

    rest = prepareString . tail $ cs

part1Solution :: [String] -> Int
part1Solution = sum . fmap extractNumber

part2Solution :: [String] -> Int
part2Solution = sum . fmap (extractNumber . prepareString)
