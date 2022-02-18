module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlphaNum)
import Data.Map (Map (..), fromList, unionsWith)
import qualified Data.Map as M
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Object = Object {unObject :: String} deriving (Eq, Ord, Show)

com = Object "COM"

instance Read Object where
  readPrec = fmap Object . lift . P.munch1 $ isAlphaNum

newtype OrbitMap = OrbitMap {unMap :: Map Object [Object]} deriving (Show)

instance Read OrbitMap where
  readPrec = fmap (OrbitMap . unionsWith (++)) . lift . P.many1 $ do
    o1 <- PC.readPrec_to_P readPrec 0
    P.char ')'
    o2 <- PC.readPrec_to_P readPrec 0
    P.skipSpaces
    return . fromList $ [(o1, [o2])]

orbits :: OrbitMap -> Int
orbits om = doer 0 com
  where
    doer :: Int -> Object -> Int
    doer depth o = case o `M.lookup` unMap om of
      Just os -> depth + (sum . map (doer (depth + 1))) os
      _ -> depth

children :: Object -> OrbitMap -> [Object]
children o om = case o `M.lookup` unMap om of
  Just os -> os ++ concatMap (`children` om) os
  _ -> []

isCommonAncestor :: Object -> Object -> Object -> OrbitMap -> Bool
isCommonAncestor ao o1 o2 om = o1 `elem` os && o2 `elem` os
  where
    os = children ao om

firstCommonAncestor :: Object -> Object -> OrbitMap -> Object
firstCommonAncestor o1 o2 om = head . doer $ com
  where
    doer :: Object -> [Object]
    doer o
      | isCommonAncestor o o1 o2 om = case o `M.lookup` unMap om of
        Just os
          | null aos -> [o]
          | otherwise -> aos
          where
            aos = concatMap doer os
        _ -> error "something is wrong"
      | otherwise = []

pathLength :: Object -> Object -> OrbitMap -> Maybe Int
pathLength o1 o2 om
  | o1 == o2 = Just 0
  | otherwise = case o1 `M.lookup` unMap om of
    Just os -> fmap (+ 1) . maximum . map (\o -> pathLength o o2 om) $ os
    _ -> Nothing

part1Solution :: OrbitMap -> Int
part1Solution = orbits

part2Solution :: OrbitMap -> Maybe Int
part2Solution om = do
  let san = Object "SAN"
      you = Object "YOU"
      comAnc = firstCommonAncestor san you om
  ps <- pathLength comAnc san om
  py <- pathLength comAnc you om
  return (ps + py - 2)
