module Lib
  ( part1Solution,
    part2Solution,
    parseTask,
  )
where

import Data.Char (isDigit)
import Data.List (foldl', transpose)
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Op = Op {opN :: Int, opFrom :: Int, opTo :: Int} deriving (Show)

instance Read Op where
  readPrec = do
    _ <- lift . P.string $ "move"
    lift P.skipSpaces
    n <- readPrec
    lift P.skipSpaces
    _ <- lift . P.string $ "from"
    lift P.skipSpaces
    from <- readPrec
    lift P.skipSpaces
    _ <- lift . P.string $ "to"
    lift P.skipSpaces
    Op n from <$> readPrec

data Task = Task {tStacks :: [String], tOps :: [Op]} deriving (Show)

parseTask :: String -> Task
parseTask cs = Task stacks ops
  where
    [stackString, opsString] = splitOn "\n\n" cs
    stacks = map (reverse . filter (/= ' ') . tail) . filter (isDigit . head) . map reverse . transpose . lines $ stackString
    ops = map read . lines $ opsString

applyOp :: Bool -> [String] -> Op -> [String]
applyOp shouldReverse stacks (Op n from to) =
  [ if i == (from - 1)
      then drop n cs
      else if i == (to - 1) then toMove <> cs else cs
    | i <- [0 .. length stacks - 1],
      let cs = stacks !! i
  ]
  where
    toMove = (if shouldReverse then reverse else id) . take n . (!! (from - 1)) $ stacks

applyOps :: Bool -> Task -> [String]
applyOps shouldReverse t = foldl' (applyOp shouldReverse) (tStacks t) . tOps $ t

part1Solution :: Task -> String
part1Solution = concatMap (take 1) . applyOps True

part2Solution :: Task -> String
part2Solution = concatMap (take 1) . applyOps False
