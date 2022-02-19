module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', intercalate)
import Data.Vector (Vector, fromList, (!))
import qualified Data.Vector as V
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

imageWidth = 25

imageHeight = 6

data Image = Image {iData :: Vector Int, iWidth :: Int, iHeight :: Int, iDepth :: Int}

instance Show Image where
  show im =
    intercalate
      "\n\n"
      [intercalate "\n" [[charColor im x y z | x <- [0 .. iWidth im - 1]] | y <- [0 .. iHeight im - 1]] | z <- [0 .. iDepth im - 1]]

mkImage :: [Int] -> Int -> Int -> Image
mkImage ns w h = Image vns w h (V.length vns `div` (w * h))
  where
    vns = fromList ns

color :: Image -> Int -> Int -> Int -> Int
color im x y z = iData im ! (x + iWidth im * y + iWidth im * iHeight im * z)

charColor :: Image -> Int -> Int -> Int -> Char
charColor im x y z = case color im x y z of
  0 -> ' '
  1 -> '#'
  _ -> '.'

processLayer :: ([Int] -> Int -> a) -> Image -> Int -> a
processLayer f im z = f [color im x y z | x <- [0 .. iWidth im - 1], y <- [0 .. iHeight im - 1]] z

processLayers :: ([Int] -> Int -> a) -> Image -> [a]
processLayers f im = [processLayer f im z | z <- [0 .. iDepth im - 1]]

flattenPixel :: [Int] -> Int
flattenPixel = foldl' (\c n -> if c == 2 then n else c) 2

flattenImage :: Image -> Image
flattenImage im =
  Image
    (fromList [flattenPixel [color im x y z | z <- [0 .. iDepth im - 1]] | y <- [0 .. iHeight im - 1], x <- [0 .. iWidth im - 1]])
    (iWidth im)
    (iHeight im)
    1

part1Solution :: [Int] -> Int
part1Solution ns =
  processLayer (\ns' _ -> (length . filter (== 1)) ns' * (length . filter (== 2)) ns') im
    . snd
    . minimum
    . processLayers (\ns' z -> ((length . filter (== 0)) ns', z))
    $ im
  where
    im = mkImage ns imageWidth imageHeight

part2Solution :: [Int] -> String
part2Solution ns = show . flattenImage $ mkImage ns imageWidth imageHeight
