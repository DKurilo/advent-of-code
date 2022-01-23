module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Entity = Group Stream | Garbage String deriving (Show)

instance Read Entity where
  readPrec =
    lift
      ( do
          P.char '{'
          s <- PC.readPrec_to_P readPrec 0
          P.char '}'
          return . Group $ s
      )
      PC.+++ lift
        ( do
            P.char '<'
            cs <-
              fmap concat . P.many $
                ( do
                    c1 <- P.char '!'
                    c2 <- P.get
                    return ""
                )
                  P.<++ P.munch1 (\c -> c /= '>' && c /= '!')
            P.char '>'
            return $ Garbage cs
        )

newtype Stream = Stream [Entity] deriving (Show)

instance Read Stream where
  readPrec = fmap Stream . lift . P.many $ do
    e <- PC.readPrec_to_P readPrec 0
    P.optional . P.char $ ','
    return e

getEntityScore :: Int -> Entity -> Int
getEntityScore n (Group s) = n + getStreamScore (n + 1) s
getEntityScore _ (Garbage _) = 0

getStreamScore :: Int -> Stream -> Int
getStreamScore n (Stream es) = (sum . map (getEntityScore n)) es

getScore :: Stream -> Int
getScore = getStreamScore 1

getEntityGarbageLength :: Entity -> Int
getEntityGarbageLength (Group s) = getGarbageLength s
getEntityGarbageLength (Garbage cs) = length cs

getGarbageLength :: Stream -> Int
getGarbageLength (Stream es) = sum . map getEntityGarbageLength $ es

part1Solution :: Stream -> Int
part1Solution = getScore

part2Solution :: Stream -> Int
part2Solution = getGarbageLength
