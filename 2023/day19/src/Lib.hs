module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Char (isAlphaNum)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Part = Part {px :: Int, pm :: Int, pa :: Int, ps :: Int} deriving (Show)

instance Read Part where
  readPrec =
    lift $ do
      _ <- P.string "{x="
      x <- readPrec_to_P readPrec 0
      _ <- P.string ",m="
      m <- readPrec_to_P readPrec 0
      _ <- P.string ",a="
      a <- readPrec_to_P readPrec 0
      _ <- P.string ",s="
      s <- readPrec_to_P readPrec 0
      _ <- P.char '}'
      return (Part x m a s)

weight :: Part -> Int
weight p = px p + pm p + pa p + ps p

data Result = ToCond String | Accept | Reject | ToNext deriving (Show, Eq)

data Cond = Cond {cName :: String, cCond :: Part -> Result}

instance Read Cond where
  readPrec =
    lift $ do
      name <- P.munch isAlphaNum
      _ <- P.char '{'
      cs <- P.many $ do
        ( do
            subj <- P.char 'x' P.<++ P.char 'm' P.<++ P.char 'a' P.<++ P.char 's'
            ac <- P.char '>' P.<++ P.char '<'
            n <- readPrec_to_P readPrec 0
            _ <- P.char ':'
            result <-
              ( do
                  _ <- P.char 'A'
                  return Accept
                )
                P.<++ ( do
                          _ <- P.char 'R'
                          return Reject
                      )
                P.<++ ( do
                          ToCond <$> P.munch isAlphaNum
                      )
            _ <- P.char ','
            let subjGetter
                  | subj == 'x' = px
                  | subj == 'm' = pm
                  | subj == 'a' = pa
                  | otherwise = ps
                action
                  | ac == '>' = (>)
                  | otherwise = (<)
            return (\p -> if action (subjGetter p) n then result else ToNext)
          )
          P.<++ ( do
                    _ <- P.string "A}"
                    return (const Accept)
                )
          P.<++ ( do
                    _ <- P.string "R}"
                    return (const Reject)
                )
          P.<++ ( do
                    name' <- P.munch isAlphaNum
                    _ <- P.char '}'
                    return . const . ToCond $ name'
                )
      return
        ( Cond
            name
            ( \p ->
                foldl'
                  ( \a c ->
                      if a == ToNext
                        then c p
                        else a
                  )
                  ToNext
                  cs
            )
        )

applyConds :: M.Map String Cond -> String -> Part -> Result
applyConds conds cs p = case cs `M.lookup` conds of
  Just c -> case result of
    ToCond cs' -> applyConds conds cs' p
    _ -> result
    where
      result = cCond c p
  _ -> error ("no condition with name " <> cs)

part1Solution :: String -> Int
part1Solution cs =
  sum
    . fmap (weight . fst)
    . filter ((== Accept) . snd)
    . fmap (\p -> (p, applyConds conds "in" p))
    $ parts
  where
    (conds, parts) =
      bimap
        (M.fromList . fmap ((\c -> (cName c, c)) . read))
        (fmap read)
        . (\css -> (head css, (head . tail) css))
        . fmap lines
        . splitOn "\n\n"
        $ cs

data RangePart = RangePart
  { pxMin :: Int,
    pxMax :: Int,
    pmMin :: Int,
    pmMax :: Int,
    paMin :: Int,
    paMax :: Int,
    psMin :: Int,
    psMax :: Int
  }
  deriving (Show)

setX :: Int -> Int -> RangePart -> RangePart
setX xMin xMax p = p {pxMin = xMin, pxMax = xMax}

setM :: Int -> Int -> RangePart -> RangePart
setM mMin mMax p = p {pmMin = mMin, pmMax = mMax}

setA :: Int -> Int -> RangePart -> RangePart
setA aMin aMax p = p {paMin = aMin, paMax = aMax}

setS :: Int -> Int -> RangePart -> RangePart
setS sMin sMax p = p {psMin = sMin, psMax = sMax}

data RangeCond = RangeCond {rcName :: String, rcCond :: RangePart -> [(RangePart, Result)]}

instance Read RangeCond where
  readPrec =
    lift $ do
      name <- P.munch isAlphaNum
      _ <- P.char '{'
      cs <- P.many $ do
        ( do
            subj <- P.char 'x' P.<++ P.char 'm' P.<++ P.char 'a' P.<++ P.char 's'
            ac <- P.char '>' P.<++ P.char '<'
            n <- readPrec_to_P readPrec 0
            _ <- P.char ':'
            result <-
              ( do
                  _ <- P.char 'A'
                  return Accept
                )
                P.<++ ( do
                          _ <- P.char 'R'
                          return Reject
                      )
                P.<++ ( do
                          ToCond <$> P.munch isAlphaNum
                      )
            _ <- P.char ','
            let (subjMinGetter, subjMaxGetter, subjSetter)
                  | subj == 'x' = (pxMin, pxMax, setX)
                  | subj == 'm' = (pmMin, pmMax, setM)
                  | subj == 'a' = (paMin, paMax, setA)
                  | otherwise = (psMin, psMax, setS)
                (action, leftN, rightN)
                  | ac == '>' = ((>), n, n + 1)
                  | otherwise = ((<), n - 1, n)

                cond :: RangePart -> [(RangePart, Result)]
                cond p
                  | asmin && asmax = [(p, result)]
                  | asmin && not asmax = [(subjSetter smin leftN p, result), (subjSetter rightN smax p, ToNext)]
                  | not asmin && asmax = [(subjSetter smin leftN p, ToNext), (subjSetter rightN smax p, result)]
                  | otherwise = [(p, ToNext)]
                  where
                    smin = subjMinGetter p
                    smax = subjMaxGetter p
                    asmin = action smin n
                    asmax = action smax n
            return cond
          )
          P.<++ ( do
                    _ <- P.string "A}"
                    return (\p -> [(p, Accept)])
                )
          P.<++ ( do
                    _ <- P.string "R}"
                    return (\p -> [(p, Reject)])
                )
          P.<++ ( do
                    name' <- P.munch isAlphaNum
                    _ <- P.char '}'
                    return (\p -> [(p, ToCond name')])
                )
      return
        ( RangeCond
            name
            ( \p ->
                foldl'
                  ( \as c ->
                      concatMap
                        ( \a ->
                            if snd a == ToNext
                              then (c . fst) a
                              else [a]
                        )
                        as
                  )
                  [(p, ToNext)]
                  cs
            )
        )

applyRangeConds :: M.Map String RangeCond -> String -> RangePart -> [(RangePart, Result)]
applyRangeConds conds cs p = case cs `M.lookup` conds of
  Just c ->
    concatMap
      ( \(p', result) -> case result of
          ToCond cs' -> applyRangeConds conds cs' p'
          _ -> [(p', result)]
      )
      . rcCond c
      $ p
  _ -> error ("no condition with name " <> cs)

count :: RangePart -> Int
count p = (pxMax p - pxMin p + 1) * (pmMax p - pmMin p + 1) * (paMax p - paMin p + 1) * (psMax p - psMin p + 1)

part2Solution :: String -> Int
part2Solution cs =
  sum
    . fmap (count . fst)
    . filter ((== Accept) . snd)
    . applyRangeConds conds "in"
    $ RangePart 1 4000 1 4000 1 4000 1 4000
  where
    conds =
      M.fromList
        . fmap ((\c -> (rcName c, c)) . read)
        . lines
        . head
        . splitOn "\n\n"
        $ cs
