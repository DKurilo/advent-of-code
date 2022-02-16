module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Sequence (Seq (..), fromList, index, update)
import qualified Data.Sequence as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

parse :: String -> Seq Int
parse cs = fromList . read $ ("[" ++ cs ++ "]")

execute :: Seq Int -> Seq Int
execute ns = doer ns 0
  where
    doer :: Seq Int -> Int -> Seq Int
    doer ns' i
      | ns' `index` i == 99 = ns'
      | ns' `index` i == 1 = doer (applyOp (+) ns' i) (i + 4)
      | ns' `index` i == 2 = doer (applyOp (*) ns' i) (i + 4)
      | otherwise = error "oops"
    applyOp op ns' i = update (ns' `index` (i + 3)) (ref ns' (i + 1) `op` ref ns' (i + 2)) ns'
    val ns' i = ns' `index` i
    ref ns' i = ns' `index` val ns' i

setNoun :: Int -> Seq Int -> Seq Int
setNoun = update 1

setVerb :: Int -> Seq Int -> Seq Int
setVerb = update 2

setSentence :: Int -> Int -> Seq Int -> Seq Int
setSentence noun verb = setNoun noun . setVerb verb

result :: Seq Int -> Int
result = (`index` 0)

sentence :: Int -> Int -> Int
sentence noun verb = 100 * noun + verb

part1Solution :: String -> Int
part1Solution = result . execute . setSentence 12 2 . parse

part2Solution :: String -> [Int]
part2Solution cs =
  map snd
    . filter
      ((== 19690720) . fst)
    $ [ ((result . execute . setNoun noun . setVerb verb) intcode, sentence noun verb)
        | noun <- [0 .. 99],
          verb <- [0 .. 99]
      ]
  where
    intcode = parse cs
