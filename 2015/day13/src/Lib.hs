module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Data.List (permutations, zipWith3)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Table = Table (S.Set String) (M.Map (String, String) Int) deriving (Show)

readLine :: P.ReadP ((String, String), Int)
readLine = do
  name1 <- P.munch1 isAlpha
  P.string " would "
  m <- (P.string "lose" >> return (-1)) P.+++ (P.string "gain" >> return 1)
  P.char ' '
  n <- PC.readPrec_to_P readPrec 0
  P.string " happiness units by sitting next to "
  name2 <- P.munch1 isAlpha
  P.char '.'
  P.char '\n'
  return ((name1, name2), m * n)

instance Read Table where
  readPrec = do
    gs <- (lift . P.many1) readLine
    let names = S.fromList . concatMap (\((n1, n2), _) -> [n1, n2]) $ gs
    return . Table names . M.fromList $ gs

evalHappiness :: M.Map (String, String) Int -> [String] -> Int
evalHappiness hs names =
  sum . concat $
    zipWith3 (\g1 g2 g3 -> [happiness g2 g1, happiness g2 g3]) names (tail names ++ names) (drop 2 names ++ names)
  where
    happiness g1 g2 = fromMaybe 0 ((g1, g2) `M.lookup` hs)

part1Solution :: Table -> Int
part1Solution (Table ns hs) = maximum . map (evalHappiness hs . (g :)) . permutations $ gs
  where
    (g : gs) = S.toList ns

part2Solution :: Table -> Int
part2Solution (Table ns hs) = maximum . map (evalHappiness hs' . ("Me" :)) . permutations $ gs
  where
    gs = S.toList ns
    hs' = foldl (\hs'' g -> M.insert (g, "Me") 0 . M.insert ("Me", g) 0 $ hs'') hs gs
