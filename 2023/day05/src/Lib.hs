module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl')
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Interval = Interval {iDest :: Int, iSource :: Int, iLength :: Int} deriving (Show)

instance Read Interval where
  readPrec = do
    dst <- readPrec
    lift P.skipSpaces
    src <- readPrec
    lift P.skipSpaces
    Interval dst src <$> readPrec

newtype Seeds = Seeds {unSeed :: [Int]} deriving (Show)

instance Read Seeds where
  readPrec = do
    _ <- lift . P.string $ "seeds:"
    lift P.skipSpaces
    Seeds
      <$> ( lift . P.many $ do
              x <- readPrec_to_P readPrec 0
              P.skipSpaces
              return x
          )

data AMap = AMap {amSrc :: String, amDst :: String, amIntervals :: [Interval]} deriving (Show)

instance Read AMap where
  readPrec = lift $ do
    src <- P.munch (\c -> c /= '-' && c /= '\n')
    _ <- P.string "-to-"
    dst <- P.munch (/= ' ')
    P.skipSpaces
    _ <- P.string "map:"
    P.skipSpaces
    AMap src dst
      <$> P.many
        ( do
            i <- readPrec_to_P readPrec 0
            P.skipSpaces
            return i
        )

data Almanac = Almanac {aSeeds :: Seeds, aMaps :: [AMap]} deriving (Show)

instance Read Almanac where
  readPrec = do
    seeds <- readPrec
    Almanac seeds
      <$> ( lift . P.many $ do
              m <- readPrec_to_P readPrec 0
              P.skipSpaces
              return m
          )

applyMap :: Int -> AMap -> Int
applyMap x m
  | null is = x
  | otherwise = iDest i + x - iSource i
  where
    is = filter (\i' -> x >= iSource i' && x <= iSource i' + iLength i' - 1) . amIntervals $ m
    i = head is

seedToLocation :: Int -> Almanac -> Int
seedToLocation x = foldl' applyMap x . aMaps

part1Solution :: String -> Int
part1Solution cs = minimum . fmap (`seedToLocation` a) . unSeed . aSeeds $ a
  where
    a = read cs

data SeedInterval = SeedInterval {siStart :: Int, siLength :: Int} deriving (Show)

instance Read SeedInterval where
  readPrec = do
    start <- readPrec
    lift P.skipSpaces
    SeedInterval start <$> readPrec

newtype Seeds2 = Seeds2 {unSeed2 :: [SeedInterval]} deriving (Show)

instance Read Seeds2 where
  readPrec = do
    _ <- lift . P.string $ "seeds:"
    lift P.skipSpaces
    Seeds2
      <$> ( lift . P.many $ do
              x <- readPrec_to_P readPrec 0
              P.skipSpaces
              return x
          )

data Almanac2 = Almanac2 {a2Seeds2 :: Seeds2, a2Maps :: [AMap]} deriving (Show)

instance Read Almanac2 where
  readPrec = do
    seeds <- readPrec
    Almanac2 seeds
      <$> ( lift . P.many $ do
              m <- readPrec_to_P readPrec 0
              P.skipSpaces
              return m
          )

splitByInterval :: Interval -> SeedInterval -> [SeedInterval]
splitByInterval i si
  | iSource i + iLength i <= siStart si = [si]
  | siStart si + siLength si <= iSource i = [si]
  | iSource i <= siStart si && iSource i + iLength i >= siStart si + siLength si = [si]
  | siStart si < iSource i && siStart si + siLength si > iSource i + iLength i =
      [ SeedInterval {siStart = siStart si, siLength = iSource i - siStart si},
        SeedInterval {siStart = iSource i, siLength = iLength i},
        SeedInterval {siStart = iSource i + iLength i, siLength = siStart si + siLength si - iSource i - iLength i}
      ]
  | siStart si < iSource i && siStart si + siLength si <= iSource i + iLength i =
      [ SeedInterval {siStart = siStart si, siLength = iSource i - siStart si},
        SeedInterval {siStart = iSource i, siLength = siStart si + siLength si - iSource i}
      ]
  | siStart si >= iSource i && siStart si + siLength si > iSource i + iLength i =
      [ SeedInterval {siStart = siStart si, siLength = iSource i + iLength i - siStart si},
        SeedInterval {siStart = iSource i + iLength i, siLength = siStart si + siLength si - iSource i - iLength i}
      ]
  | otherwise = error "impossible!"

splitSeedInterval :: SeedInterval -> AMap -> [SeedInterval]
splitSeedInterval si = foldl' doer [si] . amIntervals
  where
    doer :: [SeedInterval] -> Interval -> [SeedInterval]
    doer sis i = concatMap (splitByInterval i) sis

seed2ToLocations :: SeedInterval -> Almanac2 -> [SeedInterval]
seed2ToLocations si =
  foldl'
    (\sis m -> fmap (\si' -> si' {siStart = applyMap (siStart si') m}) . concatMap (`splitSeedInterval` m) $ sis)
    [si]
    . a2Maps

part2Solution :: String -> Int
part2Solution cs = minimum . fmap siStart . concatMap (`seed2ToLocations` a) . unSeed2 . a2Seeds2 $ a
  where
    a = read cs
