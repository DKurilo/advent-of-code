module Lib
    ( part1solution
    , part2solution
    ) where
import           Control.Applicative          ((<|>))
import           Data.Char                    (isAlpha, isDigit)
import qualified Data.Map                     as M
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

newtype Bag = Bag String deriving (Show, Eq, Ord)

readPBag :: P.ReadP Bag
readPBag = do
        d1 <- P.munch1 isAlpha
        P.skipSpaces
        d2 <- P.munch1 isAlpha
        P.skipSpaces
        P.string "bag" <|> P.string "bags"
        return . Bag $ d1 <> " " <> d2

instance Read Bag where
    readPrec = lift readPBag

newtype Rules = Rules (M.Map Bag (M.Map Bag Int)) deriving (Show, Eq)

instance Read Rules where
    readPrec = lift $ do
        bag <- readPBag
        P.skipSpaces
        P.string "contain"
        P.skipSpaces
        bags <- (do
            P.string "no other bags."
            return [M.empty]) <|> P.many1 (do
                 n <- read <$> P.munch1 isDigit
                 P.skipSpaces
                 b <- readPBag
                 P.char ',' <|> P.char '.'
                 P.skipSpaces
                 return (M.singleton b n))
        return (Rules (M.singleton bag (foldl (<>) M.empty bags)))

instance Semigroup Rules where
    (Rules r1) <> (Rules r2) = Rules (r1 <> r2)

instance Monoid Rules where
    mempty = Rules M.empty

isBagContain :: Bag -> Rules -> Bag -> Bool
isBagContain f r@(Rules mr) s
  | f == s = False -- it can't be inside itself
  | otherwise = case s `M.lookup` mr of
                  Just bs | bs == M.empty -> False
                          | f `M.member` bs -> True
                          | otherwise -> (any (isBagContain f r) . M.keys) bs
                  Nothing -> False

findAllThatContain :: Bag -> Rules -> [Bag]
findAllThatContain b r@(Rules mr) = (filter (isBagContain b r) . M.keys) mr

amountOfBagsInside' :: Bag -> Rules -> Int
amountOfBagsInside' b r@(Rules mr) = case b `M.lookup` mr of
                                      Just bs -> M.foldrWithKey (\b' bn n -> n + bn * amountOfBagsInside' b' r) 1 bs
                                      Nothing -> 1

amountOfBagsInside :: Bag -> Rules -> Int
amountOfBagsInside b r@(Rules mr) = case b `M.lookup` mr of
                                      Just bs -> M.foldrWithKey (\b' bn n -> n + bn * amountOfBagsInside' b' r) 0 bs
                                      Nothing -> 0

input :: IO Rules
input = foldl (<>) mempty . map read . filter (not . null) . lines  <$> readFile "input"

part1solution :: IO ()
part1solution = (print . length . findAllThatContain (Bag "shiny gold")) =<< input

part2solution :: IO ()
part2solution = (print . amountOfBagsInside (Bag "shiny gold")) =<< input
