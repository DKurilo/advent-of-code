module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', intercalate)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Op = Noop | AddX Int

instance Read Op where
  readPrec =
    ( do
        _ <- lift . P.string $ "noop"
        return Noop
    )
      PC.<++ ( do
                 _ <- lift . P.string $ "addx"
                 AddX <$> readPrec
             )

data Register = Register Int [Int]

mkRegister :: Register
mkRegister = Register 1 []

registerToCycles :: Register -> [Int]
registerToCycles (Register x xs) = reverse $ x : xs

applyOp :: Op -> Register -> Register
applyOp Noop (Register x xs) = Register x (x : xs)
applyOp (AddX y) (Register x xs) = Register (x + y) (x : x : xs)

chunks :: Int -> [a] -> [[a]]
chunks n xs
  | length xs <= n = [xs]
  | otherwise = take n xs : (chunks n . drop n) xs

part1Solution :: [Op] -> Int
part1Solution =
  sum
    . map (uncurry (*))
    . filter (\(i, _) -> i `elem` [20 :: Int, 60 .. 220])
    . zip [1 ..]
    . registerToCycles
    . foldl' (flip applyOp) mkRegister

part2Solution :: [Op] -> String
part2Solution =
  intercalate "\n"
    . chunks 40
    . zipWith (\i x -> if (i `mod` 40) >= x - 1 && (i `mod` 40) <= x + 1 then '#' else ' ') [0 ..]
    . registerToCycles
    . foldl' (flip applyOp) mkRegister
