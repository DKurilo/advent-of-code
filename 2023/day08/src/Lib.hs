module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlphaNum)
import Data.List (foldl')
import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Direction = L | R deriving (Show, Eq)

instance Read Direction where
  readPrec =
    lift $
      ( do
          _ <- P.char 'L'
          return L
      )
        P.<++ ( do
                  _ <- P.char 'R'
                  return R
              )

data Network = Network {nStep :: String, nDirections :: [Direction], nNodes :: M.Map String (String, String)} deriving (Show)

parseNodeName :: P.ReadP String
parseNodeName = P.munch isAlphaNum

instance Read Network where
  readPrec = lift $ do
    ds <- P.many $ readPrec_to_P readPrec 0
    P.skipSpaces
    fmap (Network "AAA" (cycle ds) . M.fromList) . P.many $ do
      n <- parseNodeName
      P.skipSpaces
      _ <- P.char '='
      P.skipSpaces
      _ <- P.char '('
      nLeft <- parseNodeName
      _ <- P.char ','
      P.skipSpaces
      nRight <- parseNodeName
      _ <- P.char ')'
      P.skipSpaces
      return (n, (nLeft, nRight))

nextStep :: String -> Network -> String
nextStep st n = case st `M.lookup` nNodes n of
  Just (leftNode, rightNode)
    | d == L -> leftNode
    | d == R -> rightNode
  _ -> "ZZZ"
  where
    d = head . nDirections $ n

desertStep :: Network -> Network
desertStep n = n {nStep = st, nDirections = tail ds}
  where
    ds = nDirections n
    st = nextStep (nStep n) n

part1Solution :: String -> Int
part1Solution = length . takeWhile ((/= "ZZZ") . nStep) . iterate desertStep . read

setStartStep :: String -> Network -> Network
setStartStep st n = n {nStep = st}

part2Solution :: String -> Int
part2Solution networkMap = foldl' lcm (head pathesLength) (tail pathesLength)
  where
    n = read networkMap
    starts = filter ((== 'A') . last) . M.keys . nNodes $ n
    pathesLength = fmap (length . takeWhile ((/= 'Z') . last . nStep) . iterate desertStep . (`setStartStep` n)) starts
