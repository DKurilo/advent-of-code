module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import           Data.List                    (intersect)
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

type Name = String
type Range = (Int, Int)

pRange :: ReadPrec Range
pRange = do
    minV <- readPrec
    lift $ P.char '-'
    maxV <-readPrec
    return (minV, maxV)

pRanges :: ReadPrec [Range]
pRanges = do
    r <- pRange
    rs <- (do
        lift P.skipSpaces
        lift $ P.string "or"
        lift P.skipSpaces
        pRanges) <|> return []
    return (r:rs)

data Rule = Rule Name [Range] deriving (Show, Eq)

instance Read Rule where
    readPrec = do
        name <- lift $ P.munch1 (/=':')
        lift $ P.char ':'
        lift P.skipSpaces
        Rule name <$> pRanges

pRules :: ReadPrec [Rule]
pRules = do
    r <- readPrec
    rs <- (do
        lift $ P.char '\n'
        pRules) <|> return []
    return (r: rs)

newtype Ticket = Ticket [Int] deriving (Show, Eq)

pVals :: ReadPrec [Int]
pVals = do
    x <- readPrec
    xs <- (do
        lift $ P.char ','
        pVals) <|> return []
    return (x:xs)

instance Read Ticket where
    readPrec = Ticket <$> pVals

pTickets :: ReadPrec [Ticket]
pTickets = do
    t <- readPrec
    ts <- (do
        lift $ P.char '\n'
        pTickets) <|> return []
    return (t: ts)

data Input = Input [Rule] Ticket [Ticket] deriving (Show)

instance Read Input where
    readPrec = do
        rs <- pRules
        lift P.skipSpaces
        lift $ P.string "your ticket:"
        lift P.skipSpaces
        t <- readPrec
        lift P.skipSpaces
        lift $ P.string "nearby tickets:"
        lift P.skipSpaces
        Input rs t <$> pTickets

nearbyTickets :: Input -> [Ticket]
nearbyTickets (Input _ _ ts) = ts

input :: IO Input
input = read <$> readFile "input"

inRange :: Int -> Range -> Bool
inRange x (minV, maxV) = x >= minV && x <= maxV

getGoodRules :: Int -> [Rule] -> [Rule]
getGoodRules x = filter (\(Rule _ rs) -> any (inRange x) rs)

wrongNumbs :: [Rule] -> Ticket -> [Int]
wrongNumbs rs t@(Ticket xs) = foldl (\xs' -> intersect xs' . wrongNumbsForRule t) xs rs
    where wrongNumbsForRule (Ticket xs) (Rule _ rs) = foldl (\xs' x -> if any (inRange x) rs then xs' else x:xs') [] xs

goodTickets :: Input -> Input
goodTickets (Input rs t ts) = Input rs t (filter (null . wrongNumbs rs) ts)

inferRules :: [(Int, [Rule])] -> [(Int, [Rule])]
inferRules cols = go cols cols
    where go [] cols' = cols'
          go ((_, [r]):cs) cols' = go cols''' cols''
              where remove = map (\(i, rs) -> if rs == [r] then (i, rs) else (i, filter (/=r) rs))
                    cols'' = remove cols'
                    cols''' = cs ++ (remove . filter (\(_, rs) -> length rs > 1)) cols'
          go (_:cs) cols' = go cs cols'

part1solution :: IO ()
part1solution = do
   (Input rs _ ts) <- input
   print . sum . concatMap (wrongNumbs rs) $ ts

part2solution :: IO ()
part2solution = do
    (Input rs t@(Ticket xs) ts) <- goodTickets <$> input
    let cols = zipWith
                 (\i rs' -> (i, foldl (\rs' (Ticket xs') -> rs' `intersect` getGoodRules (xs' !! i) rs') rs' (t:ts)))
                 [0..(length xs - 1)] . repeat $ rs
    print . product . map (\(i,_) -> xs !! i) . filter (\(i, Rule n _: _) -> take 9 n == "departure") . inferRules $ cols
