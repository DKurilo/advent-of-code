module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', sort)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Expr = Old | Val Int | Add Expr Expr | Mul Expr Expr deriving (Show)

parseArgs :: ReadPrec Expr
parseArgs =
  ( do
      _ <- lift . P.string $ "old"
      return Old
  )
    PC.+++ ( Val <$> readPrec
           )

instance Read Expr where
  readPrec =
    ( do
        e1 <- parseArgs
        lift P.skipSpaces
        _ <- lift . P.char $ '+'
        lift P.skipSpaces
        Add e1 <$> parseArgs
    )
      PC.<++ ( do
                 e1 <- parseArgs
                 lift P.skipSpaces
                 _ <- lift . P.char $ '*'
                 lift P.skipSpaces
                 Mul e1 <$> parseArgs
             )
      <++ parseArgs

calc :: Expr -> Int -> Int
calc Old x = x
calc (Val y) _ = y
calc (Add e1 e2) x = calc e1 x + calc e2 x
calc (Mul e1 e2) x = calc e1 x * calc e2 x

data Monkey = Monkey
  { mId :: Int,
    mItems :: [Int],
    mExpr :: Expr,
    mTest :: Int,
    mTrue :: Int,
    mFalse :: Int,
    mChecks :: Int
  }
  deriving (Show)

instance Read Monkey where
  readPrec = do
    _ <- lift . P.string $ "Monkey"
    lift P.skipSpaces
    monkeyId <- readPrec
    _ <- lift . P.char $ ':'
    lift P.skipSpaces
    _ <- lift . P.string $ "Starting items:"
    lift P.skipSpaces
    xs <- lift . P.many $ do
      x <- PC.readPrec_to_P readPrec 0
      _ <-
        P.optional
          ( do
              _ <- P.optional . P.string $ ", "
              P.skipSpaces
              return ()
          )
      return x
    _ <- lift . P.string $ "Operation: new = "
    expr <- readPrec
    lift P.skipSpaces
    _ <- lift . P.string $ "Test: divisible by"
    lift P.skipSpaces
    divisor <- readPrec
    lift P.skipSpaces
    _ <- lift . P.string $ "If true: throw to monkey"
    lift P.skipSpaces
    trueMonkey <- readPrec
    lift P.skipSpaces
    _ <- lift . P.string $ "If false: throw to monkey"
    lift P.skipSpaces
    falseMonkey <- readPrec
    return $ Monkey monkeyId xs expr divisor trueMonkey falseMonkey 0

newtype Game = Game {unGame :: [Monkey]}

instance Read Game where
  readPrec =
    fmap Game . lift . P.many1 $
      ( do
          m <- PC.readPrec_to_P readPrec 0
          P.skipSpaces
          return m
      )

monkeyTurn :: (Int -> Int) -> Game -> Int -> Game
monkeyTurn f g monkeyId
  | (null . mItems) monkey = g
  | otherwise = monkeyTurn f g' monkeyId
  where
    monkey = unGame g !! monkeyId
    worry = f . calc (mExpr monkey) . head . mItems $ monkey
    monkeyId' = if worry `mod` mTest monkey == 0 then mTrue monkey else mFalse monkey
    g' =
      g
        { unGame =
            map
              ( \m ->
                  if mId m == monkeyId
                    then m {mItems = tail . mItems $ m, mChecks = 1 + mChecks m}
                    else
                      if mId m == monkeyId'
                        then m {mItems = worry : mItems m}
                        else m
              )
              . unGame
              $ g
        }

gameRound :: (Int -> Int) -> Game -> Game
gameRound f g = foldl' (monkeyTurn f) g [0 .. (length . unGame) g - 1]

part1Solution :: Game -> Int
part1Solution = product . take 2 . reverse . sort . map mChecks . unGame . (!! 20) . iterate (gameRound (`div` 3))

part2Solution :: Game -> Int
part2Solution g = product . take 2 . reverse . sort . map mChecks . unGame . (!! 10000) . iterate (gameRound (`mod` modulo)) $ g
  where
    modulo = product . map mTest . unGame $ g
