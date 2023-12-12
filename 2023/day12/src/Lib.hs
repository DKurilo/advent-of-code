module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Spring = Good | Bad | Unknown deriving (Eq, Show)

instance Read Spring where
  readPrec =
    lift $
      ( do
          _ <- P.char '?'
          return Unknown
      )
        P.<++ ( do
                  _ <- P.char '.'
                  return Good
              )
        P.<++ ( do
                  _ <- P.char '#'
                  return Bad
              )

data Row = Row {rTemplate :: [Spring], rGroups :: [Int]} deriving (Show)

instance Read Row where
  readPrec = lift $ do
    bs <- P.many $ readPrec_to_P readPrec 0
    P.skipSpaces
    fmap (Row bs) . P.many $ do
      x <- readPrec_to_P readPrec 0
      ( do
          _ <- P.char ','
          return ()
        )
        P.<++ P.eof
      return x

-- First approach
-- possibleGroups :: [Spring] -> [[Int]]
-- possibleGroups = fmap (reverse . filter (/= 0)) . doer [[0]]
--   where
--     doer :: [[Int]] -> [Spring] -> [[Int]]
--     doer xss [] = xss
--     doer xss (Good : sprs) = doer (fmap (0 :) xss) sprs
--     doer xss (Bad : sprs) = doer (fmap (\xs -> head xs + 1 : tail xs) xss) sprs
--     doer xss (Unknown : sprs) = doer xss (Good : sprs) <> doer xss (Bad : sprs)

part1Solution :: [String] -> Int
part1Solution = sum . fmap (possibleSolutions . read)

adjustRow :: Row -> Row
adjustRow r = r {rTemplate = template, rGroups = groups}
  where
    template = (concatMap (<> [Unknown]) . replicate 4 . rTemplate) r <> rTemplate r
    groups = concat . replicate 5 . rGroups $ r

possibleSolutions :: Row -> Int
possibleSolutions (Row t gs) = count 0 0
  where
    lgs = length gs - 1
    lt = length t - 1

    count :: Int -> Int -> Int
    count gn pos
      | gn > lgs || pos > lt = 0
      | otherwise =
          M.fromList
            [ ((gn', pos'), doer gn' pos')
              | gn' <- [0 .. lgs],
                pos' <- [0 .. lt]
            ]
            M.! (gn, pos)

    canPlace :: Int -> Int -> Bool
    canPlace gn pos
      | gCount > pCount = False
      | pos > 0 && pos + gCount - 1 < lt && prev /= Bad && next /= Bad && Good `notElem` places = True
      | pos > 0 && pos + gCount - 1 < lt = False
      | pos + gCount - 1 < lt && next /= Bad && Good `notElem` places = True
      | pos > 0 && prev /= Bad && Good `notElem` places = True
      | pos == 0 && pos + gCount - 1 == lt && Good `notElem` places = True
      | otherwise = False
      where
        gCount = gs !! gn
        pCount = lt - pos + 1
        prev = t !! (pos - 1)
        places = take gCount . drop pos $ t
        next = t !! (pos + gCount)

    doer :: Int -> Int -> Int
    doer gn pos
      | gn > lgs || pos > lt = 0
      | gn == lgs && pos == lt = if doable then 1 else 0
      | gn == lgs && t !! pos == Bad && Bad `elem` drop (pos + gs !! gn) t = 0
      | gn == lgs && Bad `elem` drop (pos + gs !! gn) t = countNextPos
      | gn == lgs && doable = 1 + if t !! pos == Bad then 0 else countNextPos
      | doable && t !! pos == Bad = countNextGroup
      | doable = countBoth
      | not doable && t !! pos /= Bad = countNextPos
      | otherwise = 0
      where
        doable = canPlace gn pos
        countNextPos = count gn (pos + 1)
        countNextGroup = count (gn + 1) (pos + gs !! gn + 1)
        countBoth = countNextPos + countNextGroup

part2Solution :: [String] -> Int
part2Solution = sum . fmap (possibleSolutions . adjustRow . read)
