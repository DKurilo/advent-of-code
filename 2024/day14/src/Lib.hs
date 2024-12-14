module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.Bifunctor (second)
import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Point = P
  { px :: Int
  , py :: Int
  } deriving (Eq, Ord, Show)

readSgn :: P.ReadP String
readSgn = P.option "" (P.string "-")

readSgnNumber :: P.ReadP Int
readSgnNumber = do
  s <- readSgn
  x <- P.munch1 isDigit
  return . read $ s <> x

instance Read Point where
  readPrec =
    lift $ do
      _ <- P.string "p="
      x <- readSgnNumber
      _ <- P.char ','
      P x <$> readSgnNumber

data Velocity = V
  { vx :: Int
  , vy :: Int
  } deriving (Eq, Show)

instance Read Velocity where
  readPrec =
    lift $ do
      _ <- P.string "v="
      x <- readSgnNumber
      _ <- P.char ','
      V x <$> readSgnNumber

data BR = BR
  { brW :: Int
  , brH :: Int
  , brRs :: [(Point, Velocity)]
  } deriving (Eq)

readPointAndVelocity :: P.ReadP (Point, Velocity)
readPointAndVelocity = do
  p <- PC.readPrec_to_P readPrec 0
  P.skipSpaces
  v <- PC.readPrec_to_P readPrec 0
  P.skipSpaces
  return (p, v)

instance Read BR where
  readPrec =
    lift $ do
      w <- readSgnNumber
      _ <- P.char ','
      h <- readSgnNumber
      P.skipSpaces
      rbs <- P.many1 readPointAndVelocity
      return $ BR w h rbs

linesBr :: BR -> [String]
linesBr br = [[charR (P x y) | x <- [0 .. brW br - 1]] | y <- [0 .. brH br - 1]]
  where
    rs = M.fromListWith (+) . fmap (\(p, _) -> (p, 1 :: Int)) . brRs $ br
    charR p =
      case p `M.lookup` rs of
        Just _ -> '#'
        -- Just n -> head . show $ n
        Nothing -> '.'

instance Show BR where
  show = intercalate "\n" . linesBr

moveRobotSeconds :: Int -> Int -> Int -> Point -> Velocity -> Point
moveRobotSeconds n w h p v =
  P ((vx v * n + px p) `mod` w) ((vy v * n + py p) `mod` h)

waitSeconds :: Int -> BR -> BR
waitSeconds n br =
  br
    { brRs =
        fmap (\(p, v) -> (moveRobotSeconds n (brW br) (brH br) p v, v)) . brRs
          $ br
    }

factor :: BR -> Int
factor br =
  areInQuad (\p -> px p < w2 && py p < h2)
    * areInQuad (\p -> px p > w2 && py p < h2)
    * areInQuad (\p -> px p > w2 && py p > h2)
    * areInQuad (\p -> px p < w2 && py p > h2)
  where
    w2 = brW br `div` 2
    h2 = brH br `div` 2
    areInQuad cond = length . filter (cond . fst) . brRs $ br

part1Solution :: String -> Int
part1Solution = factor . waitSeconds 100 . read

isChristmasTree :: BR -> Bool
isChristmasTree br = S.size have2OrMoreNeighbors > rl `div` 2
  where
    rl = length . brRs $ br
    rs = S.fromList . fmap fst . brRs $ br
    have2OrMoreNeighbors =
      S.filter
        (\p ->
           length
             [ ()
             | dx <- [-1 .. 1]
             , dy <- [-1 .. 1]
             , P (px p + dx) (py p + dy) `S.member` rs
             ]
             >= 3)
        rs

part2Solution :: String -> [(Int, String)]
part2Solution cs =
  fmap (second show)
    . (\nbrs ->
         let n = (fst . last) nbrs + 1
          in nbrs <> [(n, waitSeconds n br)])
    . fmap fst
    . takeWhile (not . snd)
    . fmap
        (\n ->
           let nbr = (n, waitSeconds n br)
            in (nbr, isChristmasTree . snd $ nbr))
    $ [0 ..]
  where
    br = read cs
