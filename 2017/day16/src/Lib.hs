module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', takeWhile)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Program = Program {unProgram :: Char} deriving (Eq, Show, Ord)

instance Read Program where
  readPrec = Program <$> (lift . P.satisfy) (\c -> c >= 'a' && c <= 'p')

initPrograms :: [Program]
initPrograms = map Program ['a' .. 'p']

data Move = Spin Int | Exchange Int Int | Partner Program Program deriving (Show)

instance Read Move where
  readPrec =
    ( do
        lift . P.char $ 's'
        Spin <$> readPrec
    )
      PC.<++ ( do
                 lift . P.char $ 'x'
                 a <- readPrec
                 lift . P.char $ '/'
                 Exchange a <$> readPrec
             )
      PC.<++ ( do
                 lift . P.char $ 'p'
                 a <- readPrec
                 lift . P.char $ '/'
                 Partner a <$> readPrec
             )

isPartner :: Move -> Bool
isPartner (Partner _ _) = True
isPartner _ = False

newtype Dance = Dance {unDance :: [Move]} deriving (Show)

instance Read Dance where
  readPrec = fmap Dance . lift . P.many1 $ do
    m <- PC.readPrec_to_P readPrec 0
    P.optional . P.char $ ','
    return m

move :: [Program] -> Move -> [Program]
move ps (Spin n) = take l . drop (l - n `mod` l) . cycle $ ps
  where
    l = length ps
move ps (Exchange n m) = zipWith (\i c -> if i == n then pm else if i == m then pn else c) [0 ..] ps
  where
    pn = ps !! n
    pm = ps !! m
move ps (Partner pn pm) = map (\c -> if c == pn then pm else if c == pm then pn else c) ps

part1Solution :: Dance -> String
part1Solution = map unProgram . foldl' move initPrograms . unDance

part2Solution :: Dance -> String
part2Solution d = map unProgram . (!! (1000000000 `mod` cycleLength)) . iterate (\pr -> foldl' move pr moves) $ initPrograms
  where
    moves = filter (not . isPartner) . unDance $ d
    cycleLength = (length . takeWhile (/= initPrograms) . drop 1 . iterate (\pr -> foldl' move pr moves)) initPrograms + 1
