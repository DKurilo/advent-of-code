module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.Char (isDigit)
import Data.List (intercalate)
import qualified Data.Map as M
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
isChristmasTree br =
  elem
    minimalTree
    [ fmap (take minimalTreeW . drop x) . take minimalTreeH . drop y $ pic
    | x <- [0 .. brW br - minimalTreeW]
    , y <- [0 .. brH br - minimalTreeH]
    ]
  where
    minimalTreeW = 5
    minimalTreeH = 5
    minimalTree = replicate 5 "#####"
    pic = linesBr br

part2Solution :: String -> [(Int, String)]
part2Solution cs =
  fmap (\n -> (n, show . waitSeconds n $ br))
    . take 1
    . filter (isChristmasTree . (`waitSeconds` br))
    $ [0 ..]
  where
    br = read cs
