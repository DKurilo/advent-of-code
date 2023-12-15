module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha, ord)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

applyChar :: Int -> Char -> Int
applyChar x c = ((x + ord c) * 17) `mod` 256

hash :: String -> Int
hash = foldl' applyChar 0

data Instruction = IMinus String Int | IEqual String Int Int deriving (Show)

instance Read Instruction where
  readPrec = do
    cs <- lift . P.munch $ isAlpha
    let box = hash cs
    lift $
      ( do
          _ <- P.char '-'
          return (IMinus cs box)
      )
        P.<++ ( do
                  _ <- P.char '='
                  foc <- readPrec_to_P readPrec 0
                  return (IEqual cs foc box)
              )

data Lens = Lens {lLabel :: String, lFocal :: Int, lBox :: Int} deriving (Show)

newtype Box = Box {unBox :: [Lens]} deriving (Show)

newtype LightPath = LP {unLP :: M.Map Int Box} deriving (Show)

mkLightPath :: LightPath
mkLightPath = LP {unLP = M.empty}

focusingPower :: Int -> Lens -> Int
focusingPower n l = n * (lBox l + 1) * lFocal l

boxFocusingPower :: Box -> Int
boxFocusingPower = sum . zipWith focusingPower [1 ..] . unBox

lpFocusingPower :: LightPath -> Int
lpFocusingPower = sum . fmap boxFocusingPower . M.elems . unLP

applyInstruction :: Instruction -> LightPath -> LightPath
applyInstruction (IMinus cs n) lp = lp {unLP = (doer . unLP) lp}
  where
    doer :: M.Map Int Box -> M.Map Int Box
    doer boxes = case n `M.lookup` boxes of
      Just box -> M.insert n (box {unBox = (filter ((/= cs) . lLabel) . unBox) box}) boxes
      _ -> boxes
applyInstruction (IEqual cs foc n) lp = lp {unLP = (doer . unLP) lp}
  where
    doer :: M.Map Int Box -> M.Map Int Box
    doer boxes = case n `M.lookup` boxes of
      Just box
        | (not . any ((== cs) . lLabel) . unBox) box -> M.insert n (box {unBox = unBox box <> [Lens cs foc n]}) boxes
        | otherwise ->
            M.insert
              n
              ( box
                  { unBox =
                      ( fmap
                          ( \l ->
                              if lLabel l == cs
                                then l {lFocal = foc}
                                else l
                          )
                          . unBox
                      )
                        box
                  }
              )
              boxes
      _ -> M.insert n (Box [Lens cs foc n]) boxes

part1Solution :: String -> Int
part1Solution = sum . fmap hash . splitOn "," . filter (/= '\n')

part2Solution :: String -> Int
part2Solution =
  lpFocusingPower
    . foldl' (flip applyInstruction) mkLightPath
    . fmap read
    . splitOn ","
    . filter (/= '\n')
