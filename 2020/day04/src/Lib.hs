{-# LANGUAGE LambdaCase #-}

module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import           Control.Monad                ((<=<))
import           Data.Char                    (isDigit)
import           Data.List                    (find)
import           Data.List.Split              (splitOn)
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

data ValidField = BYR String | IYR String | EYR String | HGT String | HCL String | ECL String | PID String | CID String deriving (Show)
newtype PassportFields = PassportFields [ValidField] deriving (Show)
data Passport = Passport { pbyr :: String
                         , piyr :: String
                         , peyr :: String
                         , phgt :: String
                         , phcl :: String
                         , pecl :: String
                         , ppid :: String
                         , pcid :: Maybe String
                         } deriving (Show, Eq)

data Height = IN Int | CM Int deriving (Eq)

instance Read Height where
    readPrec = lift $ do
        v <- read <$> P.munch1 isDigit
        name <- P.string "in" <|> P.string "cm"
        case name of
          "in" | checkNumber 59 76 v   -> (return . IN) v
          "cm" | checkNumber 150 193 v -> (return . CM) v
          _                            -> P.pfail

instance Show Height where
    show (IN v) = show v <> "in"
    show (CM v) = show v <> "cm"

newtype HairColor = HC String deriving (Eq)

instance Read HairColor where
    readPrec = lift $ do
        P.char '#'
        v <- P.munch1 (\c -> elem c ['0'..'9'] || elem c ['a'..'f'])
        if length v == 6 then (return . HC) v else P.pfail

instance Show HairColor where
    show (HC v) = '#':v

data EyeColor = AMB | BLU | BRN | GRY | GRN | HZL | OTH deriving (Eq)

instance Read EyeColor where
    readPrec = lift $ do
        v <- P.string "amb" <|> P.string "blu" <|> P.string "brn" <|> P.string "gry" <|> P.string "grn" <|> P.string "hzl" <|> P.string "oth"
        case v of
         "amb" -> return AMB
         "blu" -> return BLU
         "brn" -> return BRN
         "gry" -> return GRY
         "grn" -> return GRN
         "hzl" -> return HZL
         "oth" -> return OTH
         _     -> P.pfail

instance Show EyeColor where
    show AMB = "amb"
    show BLU = "blu"
    show BRN = "brn"
    show GRY = "gry"
    show GRN = "grn"
    show HZL = "hzl"
    show OTH = "oth"

newtype PassID = PassID String deriving (Eq)

instance Read PassID where
    readPrec = lift $ do
        v <- P.munch1 isDigit
        if length v == 9 then (return . PassID) v else P.pfail

instance Show PassID where
    show (PassID v) = v

parseValidField :: P.ReadP ValidField
parseValidField = do
    P.skipSpaces
    k <- P.munch1 (/=':')
    P.char ':'
    P.skipSpaces
    v <- P.munch1 (\c -> c /= ' ' && c /= '\n')
    P.skipSpaces
    case k of
      "byr" -> (return . BYR) v
      "iyr" -> (return . IYR) v
      "eyr" -> (return . EYR) v
      "hgt" -> (return . HGT) v
      "hcl" -> (return . HCL) v
      "ecl" -> (return . ECL) v
      "pid" -> (return . PID) v
      "cid" -> (return . CID) v
      _     -> P.pfail

instance Read ValidField where
    readPrec = lift parseValidField

instance Read PassportFields where
    readPrec = PassportFields <$> lift (P.many parseValidField)

getPassport :: PassportFields -> Maybe Passport
getPassport (PassportFields pfs) = case (mbbyr, mbiyr, mbeyr, mbhgt, mbhcl, mbecl, mbpid) of
    (Just (BYR byr), Just (IYR iyr), Just (EYR eyr), Just (HGT hgt), Just (HCL hcl), Just (ECL ecl), Just (PID pid)) ->
        Just $ Passport byr iyr eyr hgt hcl ecl pid mbcid
    _ -> Nothing
    where mbbyr = find (\case {BYR _ -> True; _ -> False}) pfs
          mbiyr = find (\case {IYR _ -> True; _ -> False}) pfs
          mbeyr = find (\case {EYR _ -> True; _ -> False}) pfs
          mbhgt = find (\case {HGT _ -> True; _ -> False}) pfs
          mbhcl = find (\case {HCL _ -> True; _ -> False}) pfs
          mbecl = find (\case {ECL _ -> True; _ -> False}) pfs
          mbpid = find (\case {PID _ -> True; _ -> False}) pfs
          mbcid = (\(CID cid) -> cid) <$> find (\case {CID _ -> True; _ -> False}) pfs

data StrictPassport = StrictPassport { spbyr :: Int
                                     , spiyr :: Int
                                     , speyr :: Int
                                     , sphgt :: Height
                                     , sphcl :: HairColor
                                     , specl :: EyeColor
                                     , sppid :: PassID
                                     , spcid :: Maybe String
                                     } deriving (Eq)

instance Show StrictPassport where
    show sp = "byr: " <> (show . spbyr) sp
           <> "\tiyr: " <> (show . speyr) sp
           <> "\teyr: " <> (show . speyr) sp
           <> "\thgt: " <> (show . sphgt) sp
           <> "\thcl: " <> (show . sphcl) sp
           <> "\tecl: " <> (show . specl) sp
           <> "\tpid: " <> (show . sppid) sp
           <> case spcid sp of
                Just v -> "\tcid: " <> v
                _      -> ""

checkNumber :: Int -> Int -> Int -> Bool
checkNumber mn mx n = n >= mn && n <= mx

getNumber :: Int -> Int -> Int -> Maybe Int
getNumber mn mx n
  | checkNumber mn mx n = Just n
  | otherwise = Nothing

getStrictPassport :: Passport -> Maybe StrictPassport
getStrictPassport p = case (getBYR, getIYR, getEYR, getHGT, getHCL, getECL, getPID) of
                        (Just b, Just i, Just e, Just hg, Just hc, Just ec, Just pd) -> Just $ StrictPassport b i e hg hc ec pd (pcid p)
                        _ -> Nothing
    where getBYR = getNumber 1920 2002 =<< (readMaybe . pbyr) p
          getIYR = getNumber 2010 2020 =<< (readMaybe . piyr) p
          getEYR = getNumber 2020 2030 =<< (readMaybe . peyr) p
          getHGT = (readMaybe . phgt) p
          getHCL = (readMaybe . phcl) p
          getECL = (readMaybe . pecl) p
          getPID = (readMaybe . ppid) p

input :: IO [Maybe Passport]
input = map (getPassport <=< readMaybe) . splitOn "\n\n" <$> readFile "./input"

part1solution :: IO ()
part1solution = print . length . filter (/=Nothing) =<< input

part2solution :: IO ()
part2solution = print . length -- putStrLn . intercalate "\n" . map (\case {Just sp -> show sp; _ -> ""})
              . filter (/=Nothing) . map (getStrictPassport =<<) =<< input
