module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Pipes = Pipes {pPipes :: M.Map Int [Int]} deriving (Show)

instance Read Pipes where
  readPrec = do
    pr <- readPrec
    lift $ do
      P.skipSpaces
      P.string "<->"
      P.skipSpaces
    fmap (Pipes . M.fromListWith (++) . concatMap (\pr' -> [(pr, [pr']), (pr', [pr])])) . lift . P.many $ do
      pr' <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      P.optional . P.char $ ','
      P.skipSpaces
      return pr'

groupWith :: Int -> Pipes -> S.Set Int
groupWith n (Pipes pipes) = doer S.empty n
  where
    doer ns n'
      | n' `S.member` ns = S.empty
      | otherwise = case n' `M.lookup` pipes of
        Just ps -> foldl' (\ns'' p -> S.union ns'' (doer ns'' p)) ns' ps
        _ -> ns'
      where
        ns' = S.insert n' ns

part1Solution :: [Pipes] -> Int
part1Solution = S.size . groupWith 0 . Pipes . M.unionsWith (++) . map pPipes

groups :: Pipes -> [S.Set Int]
groups p@(Pipes pipes) = doer (S.fromList . M.keys $ pipes) []
  where
    doer :: S.Set Int -> [S.Set Int] -> [S.Set Int]
    doer ps grs
      | S.null ps = grs
      | otherwise = doer ps' (gr : grs)
      where
        gr = groupWith (S.elemAt 0 ps) p
        ps' = ps S.\\ gr

part2Solution :: [Pipes] -> Int
part2Solution = length . groups . Pipes . M.unionsWith (++) . map pPipes
