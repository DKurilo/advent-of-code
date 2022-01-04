module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Routes = Routes (S.Set String) (M.Map (String, String) Int) deriving (Show)

cities :: Routes -> [String]
cities (Routes cs _) = S.toList cs

routes (Routes _ rs) = rs

readRoute :: P.ReadP (String, String, Int)
readRoute = do
  let readCity = P.munch1 (/= ' ')
  cs1 <- readCity
  P.string " to "
  cs2 <- readCity
  P.string " = "
  d <- PC.readPrec_to_P readPrec 0
  P.char '\n'
  return (cs1, cs2, d)

instance Read Routes where
  readPrec = do
    rs <- (lift . P.many1) readRoute
    let cities = foldl (\cs (c1, c2, _) -> S.insert c1 . S.insert c2 $ cs) S.empty rs
        routes = foldl (\routes' (c1, c2, d) -> M.insert (c1, c2) d . M.insert (c2, c1) d $ routes') M.empty rs
    return $ Routes cities routes

weightedPerm :: (Show a, Eq a) => (a -> a -> Maybe Int) -> [a] -> [(Int, [a])]
weightedPerm fd xs = concatMap (\x -> doer 0 [x] x (filter (/= x) xs)) xs
  where
    doer d xs' _ [] = [(d, xs')]
    doer d xs' x xs'' =
      concatMap
        ( \x' -> case fd x x' of
            Just d' -> doer (d + d') (x' : xs') x' (filter (/= x') xs'')
            _ -> []
        )
        xs''

allRoutes :: Routes -> [(Int, [String])]
allRoutes r = weightedPerm (\c1 c2 -> M.lookup (c1, c2) . routes $ r) (cities r)

part1Solution :: Routes -> Int
part1Solution = minimum . map fst . allRoutes

part2Solution :: Routes -> Int
part2Solution = maximum . map fst . allRoutes
