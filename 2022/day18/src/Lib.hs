module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Cube = Cube {cX :: Int, cY :: Int, cZ :: Int} deriving (Eq, Ord, Show)

instance Read Cube where
  readPrec = do
    x <- readPrec
    _ <- lift . P.char $ ','
    y <- readPrec
    _ <- lift . P.char $ ','
    Cube x y <$> readPrec

sides :: Cube -> [Cube]
sides c =
  [ c {cX = cX c - 1},
    c {cX = cX c + 1},
    c {cY = cY c - 1},
    c {cY = cY c + 1},
    c {cZ = cZ c - 1},
    c {cZ = cZ c + 1}
  ]

exteriorMap :: [Cube] -> S.Set Cube
exteriorMap cs = doer cStart (S.singleton cStart)
  where
    cubes = S.fromList cs
    xs = map cX cs
    minX = minimum xs - 1
    maxX = maximum xs + 1
    ys = map cY cs
    minY = minimum ys - 1
    maxY = maximum ys + 1
    zs = map cZ cs
    minZ = minimum zs - 1
    maxZ = maximum zs + 1
    cStart = Cube minX minY minZ
    doer :: Cube -> S.Set Cube -> S.Set Cube
    doer c visited
      | cX c < minX || cX c > maxX = visited
      | cY c < minY || cY c > maxY = visited
      | cZ c < minZ || cZ c > maxZ = visited
      | c `S.member` cubes = visited
      | otherwise =
        foldl'
          ( \visited' c' ->
              if c' `S.member` visited'
                then visited'
                else doer c' visited'
          )
          (S.insert c visited)
          . sides
          $ c

part1Solution :: [Cube] -> Int
part1Solution cs = length . filter (`S.notMember` cubes) . concatMap sides $ cs
  where
    cubes = S.fromList cs

part2Solution :: [Cube] -> Int
part2Solution cs = length . filter (`S.member` exterior) . concatMap sides $ cs
  where
    exterior = exteriorMap cs
