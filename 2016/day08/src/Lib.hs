module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (intercalate, nub)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Op = Rect Int Int | RR Int Int | RC Int Int

instance Read Op where
  readPrec =
    ( do
        lift . P.string $ "rect "
        x <- readPrec
        lift . P.char $ 'x'
        Rect x <$> readPrec
    )
      PC.+++ ( do
                 lift . P.string $ "rotate row y="
                 y <- readPrec
                 lift . P.string $ " by "
                 RR y <$> readPrec
             )
      PC.+++ ( do
                 lift . P.string $ "rotate column x="
                 x <- readPrec
                 lift . P.string $ " by "
                 RC x <$> readPrec
             )

data Keycard = KC Int Int [Op]

instance Read Keycard where
  readPrec = do
    x <- readPrec
    lift . P.char $ 'x'
    y <- readPrec
    lift . P.char $ '\n'
    KC x y
      <$> ( lift . P.many1 $ do
              op <- PC.readPrec_to_P readPrec 0
              P.char '\n'
              return op
          )

applyOp :: Int -> Int -> [(Int, Int)] -> Op -> [(Int, Int)]
applyOp maxX maxY board (Rect x y) = nub ([(x', y') | x' <- [0 .. (x - 1)], x' < maxX, y' <- [0 .. (y - 1)], y' < maxY] ++ board)
applyOp maxX maxY board (RR y n) = map (\(x', y') -> if y' == y then ((x' + n) `mod` maxX, y') else (x', y')) board
applyOp maxX maxY board (RC x n) = map (\(x', y') -> if x' == x then (x', (y' + n) `mod` maxY) else (x', y')) board

getCode :: Keycard -> [(Int, Int)]
getCode (KC x y ops) = foldl (applyOp x y) [] ops

showBoard :: [(Int, Int)] -> String
showBoard board = intercalate "\n" [[if (x, y) `elem` board then '#' else ' ' | x <- [0 .. maxX]] | y <- [0 .. maxY]]
  where
    maxX = maximum . map fst $ board
    maxY = maximum . map snd $ board

part1Solution :: Keycard -> Int
part1Solution = length . getCode

part2Solution :: Keycard -> String
part2Solution = showBoard . getCode
