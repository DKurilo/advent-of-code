module Lib
  ( part1Solution,
    part2Solution,
    CaveSystem (..),
  )
where

import Data.Char (isAlpha, isLower, isUpper)
import Data.List (nub)
import Data.Map (Map (..), fromListWith, size)
import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Cave = Start | Small String | Big String | End deriving (Eq, Ord, Show)

isBig :: Cave -> Bool
isBig (Big _) = True
isBig _ = False

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False

isStartEnd :: Cave -> Bool
isStartEnd Start = True
isStartEnd End = True
isStartEnd _ = False

notBig :: Cave -> Bool
notBig (Big _) = False
notBig _ = True

instance Read Cave where
  readPrec =
    lift
      ( ( do
            P.string "start"
            return Start
        )
          P.<++ ( do
                    P.string "end"
                    return End
                )
          P.<++ (Small <$> P.munch (\c -> isAlpha c && isLower c))
          P.+++ (Big <$> P.munch (\c -> isAlpha c && isUpper c))
      )

newtype CaveSystem = CS (Map Cave [Cave]) deriving (Eq, Show)

instance Read CaveSystem where
  readPrec =
    CS . fromListWith (++) . concat
      <$> ( lift . P.many $ do
              c1 <- PC.readPrec_to_P readPrec 0
              P.char '-'
              c2 <- PC.readPrec_to_P readPrec 0
              P.char '\n'
              return [(c1, [c2]), (c2, [c1])]
          )

paths1 :: Cave -> Cave -> [Cave] -> CaveSystem -> [[Cave]]
paths1 start end p caveSystem@(CS m)
  | start == end = [[end]]
  | otherwise = maybe [] paths' (start `M.lookup` m)
  where
    paths' =
      map (start :) . filter (not . null)
        . concatMap
          ( \c ->
              if c `elem` p && notBig c
                then []
                else paths1 c end (start : p) caveSystem
          )

part1Solution :: CaveSystem -> Int
part1Solution = length . paths1 Start End []

paths2 :: Cave -> Cave -> [Cave] -> [Cave] -> CaveSystem -> [[Cave]]
paths2 start end p s caveSystem@(CS m)
  | start == end && length s == 1 && (length . filter (== head s)) p == 1 = [] -- deduplicate paths
  | start == end = [[end]]
  | otherwise = maybe [] paths' (start `M.lookup` m)
  where
    paths' =
      map (start :) . filter (not . null)
        . concatMap remainingPaths
    remainingPaths :: Cave -> [[Cave]]
    remainingPaths c
      | isBig c = paths2 c end (start : p) s caveSystem
      | isSmall c && null s && c `notElem` p = paths2 c end (start : p) [] caveSystem ++ paths2 c end (start : p) [c] caveSystem
      | isSmall c && length s == 1 && c == head s = paths2 c end (start : p) (c : s) caveSystem
      | isSmall c && c `notElem` p && c `notElem` s = paths2 c end (start : p) s caveSystem
      | isStartEnd c && c `notElem` p = paths2 c end (start : p) s caveSystem
      | otherwise = []

part2Solution :: CaveSystem -> Int
part2Solution = length . paths2 Start End [] []
