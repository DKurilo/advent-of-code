module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Text.ParserCombinators.ReadP as P
import Text.Read
import Data.Char (isDigit)

readNumber :: Int -> P.ReadP Int
readNumber = fmap read . doer
  where
      doer :: Int -> P.ReadP String
      doer 0 = return []
      doer n = do
          c <- P.satisfy isDigit
          cs <- doer (n - 1) P.<++ return []
          return $ c:cs
  

data Op = Mul Int Int | Do | Dont deriving (Eq, Show)

instance Read Op where
    readPrec = lift $ (do
          _ <- P.string "mul("
          x <- readNumber 3
          _ <- P.char ','
          y <- readNumber 3
          _ <- P.char ')'
          return $ Mul x y) P.<++ (do
              _ <- P.string "do()"
              return Do
          ) P.<++ (do
              _ <- P.string "don't()"
              return Dont
          )

newtype Ops = Ops [Op] deriving (Show)

readOps :: P.ReadP [Op]
readOps = (do
  op <- readPrec_to_P readPrec 0
  ops <- readOps
  return $ op:ops) P.<++ (do
    _ <- P.get
    readOps
  ) P.<++ (do
    _ <- P.many P.get
    return []
  )

instance Read Ops where
  readPrec = lift . fmap Ops $ readOps

calc :: Ops -> Int
calc (Ops ops) = sum . doer True $ ops
    where
        doer :: Bool -> [Op] -> [Int]
        doer _ [] = []
        doer _ (Do:ops') = doer True ops'
        doer _ (Dont:ops') = doer False ops'
        doer False (_:ops') = doer False ops'
        doer True ((Mul x y):ops') = (x * y) : doer True ops'

part1Solution :: String -> Int
part1Solution = calc . (\(Ops ops) -> Ops . filter (/= Dont) $ ops) . read

part2Solution :: String -> Int
part2Solution = calc . read
