module Lib (
    part1Solution,
    part2Solution,
)
where

import Data.List (sortOn, tails)
import Data.Ord (Down (..))
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read
import Debug.Trace (trace)

data Ind = On | Off deriving (Eq, Show)

instance Read Ind where
    readPrec =
        lift $
            ( do
                _ <- P.char '.'
                return Off
            )
                P.+++ ( do
                            _ <- P.char '#'
                            return On
                      )

readInds = do
    _ <- lift . P.char $ '['
    inds <- lift . P.many1 . PC.readPrec_to_P readPrec $ 0
    _ <- lift . P.char $ ']'
    return inds

pressInd :: Ind -> Ind
pressInd On = Off
pressInd Off = On

newtype Button = Button {bInds :: [Int]} deriving (Eq, Show)

instance Read Button where
    readPrec = lift $ do
        P.skipSpaces
        _ <- P.char '('
        xs <- P.many1 $ do
            x <- PC.readPrec_to_P readPrec 0
            _ <- P.optional . P.char $ ','
            return x
        _ <- P.char ')'
        return $ Button xs

readButtons = lift . P.many $ do
    b <- PC.readPrec_to_P readPrec 0
    P.skipSpaces
    return b

newtype Jolt = Jolt {jV :: Int} deriving (Eq, Show)

instance Read Jolt where
    readPrec = Jolt <$> readPrec

readJolts = do
    _ <- lift . P.char $ '{'
    js <- lift . P.many1 $ do
        j <- PC.readPrec_to_P readPrec 0
        _ <- P.optional . P.char $ ','
        return j
    _ <- lift . P.char $ '}'
    return js

data Machine = Machine {mInd :: [Ind], mButtons :: [Button], mJolts :: [Jolt]} deriving (Eq, Show)

instance Read Machine where
    readPrec = do
        inds <- readInds
        lift P.skipSpaces
        bs <- readButtons
        lift P.skipSpaces
        Machine inds bs <$> readJolts

pressButton :: [Ind] -> Button -> [Ind]
pressButton is = foldl' (\is' n -> zipWith (\n' i -> if n' == n then pressInd i else i) [0 ..] is') is . bInds

kFromN :: Int -> [a] -> [[a]]
kFromN k xs
    | k < 0 = []
    | k == 0 = [[]]
    | otherwise = concatMap (\(y : ys) -> fmap (y :) (kFromN (k - 1) ys)) . init . tails $ xs

findButtons :: Machine -> [Button]
findButtons m = doer 0
  where
    initInds = replicate (length . mInd $ m) Off

    doer :: Int -> [Button]
    doer k
        | null applied = doer (k + 1)
        | otherwise = fst . head $ applied
      where
        presses = kFromN k . mButtons $ m
        applied = filter ((== mInd m) . snd) . fmap (\bs -> (bs, foldl' pressButton initInds bs)) $ presses

part1Solution :: [Machine] -> Int
part1Solution = sum . fmap (length . findButtons)

pressPowerButton :: Int -> [Int] -> Button -> [Int]
pressPowerButton k is = foldl' (\is' n -> zipWith (\n' i -> if n' == n then i + k else i) [0 ..] is') is . bInds

findPowerButtons :: Machine -> [Int]
findPowerButtons m = trace (show sortBut) $ doer initBut
  where
    initPow = replicate (length . mJolts $ m) 0
    initBut = replicate (length . mButtons $ m) 0
    sortBut = sortOn (Down . length . bInds) . mButtons $ m

    pressButs :: [Int] -> [Int]
    pressButs xs = foldl' (\pow (k, b) -> pressPowerButton k pow b) initPow . zip xs $ sortBut

    isOverflow :: [Int] -> Bool
    isOverflow = or . zipWith (<) (fmap jV . mJolts $ m)

    add1 :: [Int] -> Int -> [Int]
    add1 buts j
        | isOverflow buts' = add1 (zipWith (\i b -> if i <= j then 0 else b) [0 ..] buts) (j + 1)
        | otherwise = buts'
      where
        buts' = zipWith (\i b -> if i == j then b + 1 else b) [0 ..] buts

    doer :: [Int] -> [Int]
    doer buts
        | pows == (fmap jV . mJolts) m = buts
        | otherwise = doer (add1 buts 0)
      where
        pows = pressButs buts

part2Solution :: [Machine] -> Int
part2Solution = sum . fmap ((\x -> trace (show x) x) . sum . findPowerButtons)
