module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Command = Forward Int | Down Int | Up Int

instance Read Command where
  readPrec = do
    cmd <- lift (P.string "forward" P.+++ P.string "down" P.+++ P.string "up")
    (lift . P.char) ' '
    n <- readPrec
    return $ case cmd of
      "forward" -> Forward n
      "down" -> Down n
      "up" -> Up n

part1Solution :: [Command] -> Int
part1Solution = res . foldl applyCommand (0, 0)
  where
    applyCommand :: (Int, Int) -> Command -> (Int, Int)
    applyCommand (h, d) (Forward n) = (h + n, d)
    applyCommand (h, d) (Down n) = (h, d + n)
    applyCommand (h, d) (Up n) = (h, d - n)
    res (x, y) = x * y

part2Solution :: [Command] -> Int
part2Solution = res . foldl applyCommand (0, 0, 0)
  where
    applyCommand :: (Int, Int, Int) -> Command -> (Int, Int, Int)
    applyCommand (h, d, a) (Forward n) = (h + n, d + n * a, a)
    applyCommand (h, d, a) (Down n) = (h, d, a + n)
    applyCommand (h, d, a) (Up n) = (h, d, a - n)
    res (x, y, _) = x * y
