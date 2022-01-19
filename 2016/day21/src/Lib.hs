module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Data.List (elemIndex, iterate)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Direction = L | R deriving (Eq, Show)

instance Read Direction where
  readPrec = lift $ (P.string "left" >> return L) P.+++ (P.string "right" >> return R)

data Op
  = Set String
  | SetP String
  | SwapP Int Int
  | SwapL Char Char
  | Rotate Direction Int
  | RotateP Char
  | Reverse Int Int
  | Move Int Int
  deriving (Eq, Show)

instance Read Op where
  readPrec =
    ( do
        lift . P.string $ "set string "
        Set <$> (lift . P.munch1) isAlpha
    )
      PC.+++ ( do
                 lift . P.string $ "set password "
                 SetP <$> (lift . P.munch1) isAlpha
             )
      PC.+++ ( do
                 lift . P.string $ "swap position "
                 x <- readPrec
                 lift . P.string $ " with position "
                 SwapP x <$> readPrec
             )
      PC.+++ ( do
                 lift . P.string $ "swap letter "
                 x <- lift P.get
                 lift . P.string $ " with letter "
                 SwapL x <$> lift P.get
             )
      PC.+++ ( do
                 lift . P.string $ "rotate "
                 dir <- readPrec
                 lift P.skipSpaces
                 x <- readPrec
                 lift . P.string $ " step"
                 lift . P.many . P.char $ 's'
                 return $ Rotate dir x
             )
      PC.+++ ( do
                 lift . P.string $ "rotate based on position of letter "
                 RotateP <$> lift P.get
             )
      PC.+++ ( do
                 lift . P.string $ "reverse positions "
                 x <- readPrec
                 lift . P.string $ " through "
                 Reverse x <$> readPrec
             )
      PC.+++ ( do
                 lift . P.string $ "move position "
                 x <- readPrec
                 lift . P.string $ " to position "
                 Move x <$> readPrec
             )

data Scrambler = Scrambler {scramblerPos :: Int, scramblerString :: String, scramblerOps :: [Op]} deriving (Eq, Show)

instance Read Scrambler where
  readPrec = fmap (Scrambler 0 "") . lift . P.many1 $ do
    op <- PC.readPrec_to_P readPrec 0
    P.skipSpaces
    return op

applyOp :: Scrambler -> Scrambler
applyOp s = doer (ops !! pos)
  where
    ops = scramblerOps s
    pos = scramblerPos s
    cs = scramblerString s
    doer (Set cs') = Scrambler (pos + 1) cs' ops
    doer (SetP _) = Scrambler (pos + 1) cs ops
    doer (SwapP x y) = Scrambler (pos + 1) (zipWith (\i c -> if i == x then cs !! y else if i == y then cs !! x else c) [0 ..] cs) ops
    doer (SwapL x y) = Scrambler (pos + 1) (map (\c -> if c == x then y else if c == y then x else c) cs) ops
    doer (Rotate R x) = Scrambler (pos + 1) (drop n cs ++ take n cs) ops
      where
        n = length cs - x
    doer (Rotate L x) = Scrambler (pos + 1) (drop x cs ++ take x cs) ops
    doer (RotateP c) = Scrambler (pos + 1) (drop n cs ++ take n cs) ops
      where
        (Just x) = c `elemIndex` cs
        n = length cs - (1 + x + if x >= 4 then 1 else 0) `mod` length cs
    doer (Reverse x y) = Scrambler (pos + 1) (take x cs ++ (reverse . take (y - x + 1) . drop x) cs ++ drop (y + 1) cs) ops
    doer (Move x y)
      | x == y = Scrambler (pos + 1) cs ops
      | otherwise =
        Scrambler
          (pos + 1)
          ( concat
              . zipWith move [0 ..]
              $ cs
          )
          ops
      where
        cx = cs !! x
        move i c
          | i == x = []
          | y < x && i == y || y > x && i == y + 1 = [cx, c]
          | i == y && y == length cs - 1 = [c, cx]
          | otherwise = [c]

runScrambler :: Scrambler -> Scrambler
runScrambler = until (\s -> scramblerPos s >= (length . scramblerOps) s) applyOp

part1Solution :: Scrambler -> String
part1Solution = scramblerString . runScrambler

allRotations :: String -> [String]
allRotations cs = take (length cs) . iterate (\cs' -> tail cs' ++ take 1 cs') $ cs

allPlacements :: Char -> String -> [String]
allPlacements c cs = [take n cs ++ [c] ++ drop n cs | n <- [0 .. (length cs)]]

applyUnOp :: Scrambler -> [Scrambler]
applyUnOp s = doer (ops !! pos)
  where
    ops = scramblerOps s
    pos = scramblerPos s
    cs = scramblerString s
    doer (Set _) = [Scrambler (pos - 1) cs ops]
    doer (SetP cs') = [Scrambler (pos - 1) cs' ops]
    doer (SwapP x y) = [Scrambler (pos - 1) (zipWith (\i c -> if i == x then cs !! y else if i == y then cs !! x else c) [0 ..] cs) ops]
    doer (SwapL x y) = [Scrambler (pos - 1) (map (\c -> if c == x then y else if c == y then x else c) cs) ops]
    doer (Rotate L x) = [Scrambler (pos - 1) (drop n cs ++ take n cs) ops]
      where
        n = length cs - x
    doer (Rotate R x) = [Scrambler (pos - 1) (drop x cs ++ take x cs) ops]
    doer (RotateP c) = map (\cs' -> Scrambler (pos - 1) cs' ops) . filter ((== cs) . rotp) . allRotations $ cs
      where
        rotp :: String -> String
        rotp cs' = drop n cs' ++ take n cs'
          where
            (Just x) = c `elemIndex` cs'
            n = length cs' - (1 + x + if x >= 4 then 1 else 0) `mod` length cs'
    doer (Reverse x y) = [Scrambler (pos - 1) (take x cs ++ (reverse . take (y - x + 1) . drop x) cs ++ drop (y + 1) cs) ops]
    doer (Move x y) = map (\cs' -> Scrambler (pos - 1) cs' ops) . filter ((== cs) . movef) . allPlacements c' . filter (/= c') $ cs
      where
        c' = cs !! y
        movef :: String -> String
        movef cs'
          | x == y = cs'
          | otherwise = concat . zipWith move [0 ..] $ cs'
          where
            cx = cs' !! x
            move i c
              | i == x = []
              | y < x && i == y || y > x && i == y + 1 = [cx, c]
              | i == y && y == length cs' - 1 = [c, cx]
              | otherwise = [c]

runUnScrambler :: Scrambler -> [Scrambler]
runUnScrambler = until (\scrs -> (scramblerPos . head) scrs < 0) (concatMap applyUnOp) . (: [])

unScrambler :: Scrambler -> Scrambler
unScrambler (Scrambler _ _ ops) = Scrambler (length ops - 1) "" ops

part2Solution :: Scrambler -> [String]
part2Solution = map scramblerString . runUnScrambler . unScrambler
