{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (elem, elemIndex, intersect, notElem, nub, sort, (\\))
import Data.Maybe (fromMaybe, isJust)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PR
import Text.Read

data Digit = D String (Maybe Int) deriving (Eq, Show)

instance Read Digit where
  readPrec = do
    s <- sort <$> (lift . P.munch) (/= ' ')
    return $ case length s of
      2 -> D s (Just 1)
      4 -> D s (Just 4)
      3 -> D s (Just 7)
      7 -> D s (Just 8)
      _ -> D s Nothing

data Record = R [Digit] [Digit] deriving (Eq, Show)

instance Read Record where
  readPrec = do
    ds <-
      lift . P.many1 $
        ( do
            d <- PR.readPrec_to_P readPrec 0
            P.char ' '
            return d
        )
    (lift . P.char) '|'
    o <-
      lift . P.many1 $
        ( do
            P.char ' '
            PR.readPrec_to_P readPrec 0
        )
    return $ R ds o

part1Solution :: [Record] -> Int
part1Solution = length . filter (\(D _ v) -> isJust v) . concatMap (\(R _ ds) -> ds)

digits = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

segInit = map (,"abcdefg") "abcdefg"

deduceSegments :: [(Char, String)] -> Digit -> [(Char, String)]
deduceSegments segs (D _ Nothing) = segs
deduceSegments segs (D cs (Just n)) =
  map
    ( \(c, cs') ->
        if c `elem` cs
          then (c, digits !! n `intersect` cs')
          else (c, cs')
    )
    segs

cleanUpSegments :: [(Char, String)] -> Digit -> [(Char, String)]
cleanUpSegments segs (D _ Nothing) = segs
cleanUpSegments segs (D cs (Just n)) =
  map
    ( \(c, cs') ->
        if c `notElem` cs
          then (c, cs' \\ (digits !! n))
          else (c, cs')
    )
    segs

possible :: [(Char, String)] -> [String]
possible = map sort . doer [""]
  where
    doer :: [String] -> [(Char, String)] -> [String]
    doer css [] = css
    doer css ((_, cs) : segs) = doer (concatMap (\cs' -> (map (\c -> cs' ++ [c]) . filter (`notElem` cs')) cs) css) segs

deduceDigit :: [(Char, String)] -> Digit -> Digit
deduceDigit _ d@(D _ (Just _)) = d
deduceDigit segs d@(D cs Nothing)
  | length ds == 1 = D cs (head ds `elemIndex` digits)
  | otherwise = d
  where
    ds = nub . filter (`elem` digits) . possible . filter ((`elem` cs) . fst) $ segs

deduceDigits :: [(Char, String)] -> [Digit] -> [Digit]
deduceDigits segs = map (deduceDigit segs)

getNumber :: [Digit] -> Int
getNumber = foldl (\x (D _ mbd) -> x * 10 + fromMaybe 0 mbd) 0

findOutput :: [(Char, String)] -> Record -> Int
findOutput segs (R ds os)
  | (not . any (\(D _ v) -> null v)) os' = getNumber os'
  | otherwise = findOutput segs'' (R ds' os')
  where
    segs' = foldl deduceSegments segs ds
    segs'' = foldl cleanUpSegments segs' ds
    ds' = deduceDigits segs'' ds
    os' =
      map
        ( \o@(D cs mbd) -> case mbd of
            Just _ -> o
            _ -> (head . filter (\(D cs' _) -> cs == cs')) ds'
        )
        os

part2Solution :: [Record] -> Int
part2Solution = sum . map (findOutput segInit)
