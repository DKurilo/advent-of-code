module Lib (
    part1Solution,
    part2Solution,
)
where

import Data.List (find, sortOn, tails)
import qualified Data.Matrix as MX
import Data.Maybe (isNothing)
import Data.Ord (Down (..))
import Data.Ratio (denominator, numerator)
import qualified Data.Vector as V
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

replace :: Int -> a -> [a] -> [a]
replace n e t = x <> (e : y)
  where
    (x, _ : y) = splitAt n t

rref :: MX.Matrix Rational -> MX.Matrix Rational
rref m = MX.fromLists $ f matM 0 [0 .. rows - 1]
  where
    matM = MX.toLists m
    rows = MX.nrows m
    cols = MX.ncols m

    f a _ [] = a
    f a lead (r : rs)
        | isNothing indices = a
        | otherwise = f a' (lead' + 1) rs
      where
        indices = find p l
        p (col, row) = a !! row !! col /= 0
        l =
            [ (col, row)
            | col <- [lead .. cols - 1]
            , row <- [r .. rows - 1]
            ]

        Just (lead', i) = indices
        newRow = fmap (/ a !! i !! lead') $ a !! i

        a' =
            zipWith g [0 ..] $
                replace r newRow $
                    replace i (a !! r) a
        g n row
            | n == r = row
            | otherwise = zipWith h newRow row
          where
            h = subtract . (* row !! lead')

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
        return . Button $ xs

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

data Machine = Machine {mInd :: [Ind], mButtons :: [Button], mJolts :: [(Int, Jolt)]} deriving (Eq, Show)

instance Read Machine where
    readPrec = do
        inds <- readInds
        lift P.skipSpaces
        bs <- readButtons
        lift P.skipSpaces
        Machine inds bs . zip [0 ..] <$> readJolts

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

findPowerButtons :: Machine -> Int -> [[Int]]
findPowerButtons m numberToDefine = doer initBut
  where
    -- initBut =
    --     sortOn (\(i, _) -> minimum . fmap (\j -> jV . snd . (!! j) . mJolts $ m) . bInds . (!! i) . mButtons $ m)
    --         . zip [0 ..]
    --         . replicate (length . mButtons $ m)
    --         $ 0
    initBut =
        sortOn (\(i, _) -> Down . length . bInds . (!! i) . mButtons $ m)
            . zip [0 ..]
            . replicate (length . mButtons $ m)
            $ 0
    machineBut = mButtons m

    isOverflow :: [(Int, Int)] -> Bool
    isOverflow bsCounts =
        not . null $
            [ ()
            | (i, Jolt v) <- mJolts m
            , let v' = sum . fmap (\(j, x) -> if i `elem` (bInds . (!! j)) machineBut then x else 0) $ bsCounts
            , v < v'
            ]

    add1 :: [(Int, Int)] -> Int -> Maybe [(Int, Int)]
    add1 buts j
        | j >= numberToDefine = Nothing
        | isOverflow buts' = add1 (zipWith (\i (i', b) -> if i <= j then (i', 0) else (i', b)) [0 ..] buts) (j + 1)
        | otherwise = Just buts'
      where
        buts' = zipWith (\i (i', b) -> if i == j then (i', b + 1) else (i', b)) [0 ..] buts

    doer :: [(Int, Int)] -> [[Int]]
    doer buts = case (solve buts, add1 buts 0) of
        (Just solution, Just buts') -> solution : doer buts'
        (Just solution, Nothing) -> [solution]
        (Nothing, Just buts') -> doer buts'
        (Nothing, Nothing) -> []

    isSolution :: MX.Matrix Rational -> Bool
    isSolution mx =
        length
            [ ()
            | r <- take diagW mx'
            , let r' = take diagW r
            , (length . filter (== 1)) r' == 1
            , (length . filter (== 0)) r' == diagW - 1
            ]
            == diagW
            && length
                [ ()
                | r <- drop diagW mx'
                , all (== 0) r
                ]
                == h - diagW
      where
        mx' = MX.toLists mx
        w = MX.ncols mx - 1
        h = MX.nrows mx
        diagW = min w h

    solve :: [(Int, Int)] -> Maybe [Int]
    solve addButs
        | (not . isSolution) matrix' || any (\x -> (numerator x `mod` denominator x) /= 0 || x < 0) solutions =
            Nothing
        | otherwise = Just . fmap (\x -> fromInteger $ numerator x `div` denominator x) $ solutions
      where
        results =
            MX.fromLists $
                (fmap ((: []) . toRational . snd) . take numberToDefine) addButs <> (fmap ((: []) . toRational . jV . snd) . mJolts) m
        matrix =
            MX.fromLists . fmap (fmap toRational) $
                [ replicate i 0
                    <> [1]
                    <> replicate (length machineBut - i - 1) 0
                | i <- fmap fst . take numberToDefine $ addButs
                ]
                    <> [ [ if i `elem` bInds (machineBut !! k)
                            then 1
                            else 0
                         | k <- [0 .. length machineBut - 1]
                         ]
                       | i <- (fmap fst . mJolts) m
                       ]
        matrix' = rref (matrix MX.<|> results)
        solutions = V.toList . MX.getCol (MX.ncols matrix') $ matrix'

findPowerButtons' :: Machine -> Int -> [[Int]]
findPowerButtons' m numberToDefine
    | null solutions = findPowerButtons' m (numberToDefine + 1)
    | otherwise = solutions
  where
    solutions = findPowerButtons m numberToDefine

part2Solution :: [Machine] -> Int
part2Solution = sum . fmap (minimum . fmap sum . (\m -> findPowerButtons' m (numberToDefine m)))
  where
    numberToDefine m = max 0 $ (length . mButtons) m - (length . mJolts) m
