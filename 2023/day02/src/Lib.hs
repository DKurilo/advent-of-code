module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data CubeSet = CS {csRed :: Int, csBlue :: Int, csGreen :: Int} deriving (Show)

instance Semigroup CubeSet where
  (CS r1 b1 g1) <> (CS r2 b2 g2) = CS (r1 + r2) (b1 + b2) (g1 + g2)

instance Monoid CubeSet where
  mempty = CS 0 0 0

readColor :: P.ReadP CubeSet
readColor = do
  n <- PC.readPrec_to_P readPrec 0
  P.skipSpaces
  ( do
      _ <- P.string "red"
      return (CS n 0 0)
    )
    P.<++ ( do
              _ <- P.string "blue"
              return (CS 0 n 0)
          )
    P.<++ ( do
              _ <- P.string "green"
              return (CS 0 0 n)
          )

instance Read CubeSet where
  readPrec = lift $ do
    colors <- P.many1 $ do
      c <- readColor
      (P.optional . P.char) ','
      P.skipSpaces
      return c
    return . mconcat $ colors

data Game = Game {gameId :: Int, gameCubeSets :: [CubeSet]} deriving (Show)

instance Read Game where
  readPrec = lift $ do
    _ <- P.string "Game"
    P.skipSpaces
    n <- readPrec_to_P readPrec 0
    _ <- P.char ':'
    P.skipSpaces
    cs <- P.many1 $ do
      c <- readPrec_to_P readPrec 0
      ( do
          _ <- P.char ';'
          P.skipSpaces
          return ()
        )
        P.<++ P.eof
      return c
    return (Game n cs)

isPossibleGame :: CubeSet -> Game -> Bool
isPossibleGame csMax = all (\cs -> csRed cs <= csRed csMax && csBlue cs <= csBlue csMax && csGreen cs <= csGreen csMax) . gameCubeSets

minimalSet :: Game -> CubeSet
minimalSet g = CS {csRed = maxVal csRed, csBlue = maxVal csBlue, csGreen = maxVal csGreen}
  where
    maxVal :: (CubeSet -> Int) -> Int
    maxVal f = maximum . fmap f . gameCubeSets $ g

gamePower :: Game -> Int
gamePower g = csRed minSet * csBlue minSet * csGreen minSet
  where
    minSet = minimalSet g

part1Solution :: [String] -> Int
part1Solution = sum . fmap gameId . filter (isPossibleGame (CS {csRed = 12, csBlue = 14, csGreen = 13})) . fmap read

part2Solution :: [String] -> Int
part2Solution = sum . fmap (gamePower . read)
