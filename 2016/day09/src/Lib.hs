module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data CStringPartSimple = StrS String | MarkerS Int String

readPCStringPartSimple :: P.ReadP CStringPartSimple
readPCStringPartSimple =
  ( do
      (l, n) <- P.between (P.char '(') (P.char ')') $ do
        l' <- PC.readPrec_to_P readPrec 0
        P.char 'x'
        n' <- PC.readPrec_to_P readPrec 0
        return (l', n')
      cs <- P.count l P.get
      return $ MarkerS n cs
  )
    P.<++ (StrS <$> P.munch1 (/= '('))

instance Show CStringPartSimple where
  show (StrS cs) = cs
  show (MarkerS n cs) = concat $ replicate n cs

newtype CStringSimple = CStringSimple [CStringPartSimple]

instance Read CStringSimple where
  readPrec = CStringSimple <$> (lift . P.many1) readPCStringPartSimple

instance Show CStringSimple where
  show (CStringSimple parts) = concatMap show parts

part1Solution :: CStringSimple -> Int
part1Solution = length . show

data CStringPart = Str String | Marker Int CString

readPCStringPart :: P.ReadP CStringPart
readPCStringPart =
  ( do
      (l, n) <- P.between (P.char '(') (P.char ')') $ do
        l' <- PC.readPrec_to_P readPrec 0
        P.char 'x'
        n' <- PC.readPrec_to_P readPrec 0
        return (l', n')
      cs <- P.count l P.get
      let subCString = read cs
      return $ Marker n subCString
  )
    P.<++ (Str <$> P.munch1 (/= '('))

calcLengthPart :: CStringPart -> Int
calcLengthPart (Str cs) = length cs
calcLengthPart (Marker n cstr) = n * calcLength cstr

newtype CString = CString [CStringPart]

instance Read CString where
  readPrec = CString <$> (lift . P.many1) readPCStringPart

calcLength (CString parts) = sum . map calcLengthPart $ parts

part2Solution :: CString -> Int
part2Solution = calcLength
