{-# LANGUAGE TupleSections #-}
module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import           Data.Char                    (isAlpha)
import           Data.List                    (intercalate, intersect)
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

newtype Ingredient = Ingredient String deriving (Eq, Show, Ord)

instance Read Ingredient where
    readPrec = Ingredient <$> lift (P.munch1 isAlpha)

pIngredients :: ReadPrec [Ingredient]
pIngredients = do
    i <- readPrec
    is <- (do
        lift P.skipSpaces
        pIngredients) <|> return []
    return (i:is)

newtype Allergen = Allergen String deriving (Eq, Show, Ord)

instance Read Allergen where
    readPrec = Allergen <$> lift (P.munch1 isAlpha)

pAllergens :: ReadPrec [Allergen]
pAllergens = do
    a <- readPrec
    as <- (do
        lift $ P.char ','
        lift P.skipSpaces
        pAllergens) <|> return []
    return (a:as)

data Recipe = Recipe [Ingredient] [Allergen] deriving (Eq, Show, Ord)

instance Read Recipe where
    readPrec = do
        is <- pIngredients
        as <- (do
            lift P.skipSpaces
            lift $ P.string "(contains"
            lift P.skipSpaces
            as' <- pAllergens
            lift $ P.char ')'
            return as') <|> return []
        return (Recipe is as)

allergens :: Recipe -> M.Map Allergen [Ingredient]
allergens (Recipe is as) = M.fromList . map (,is) $ as

revealAllergens :: [Recipe] -> M.Map Allergen Ingredient
revealAllergens rs = cleanup (M.keys mas) mas
    where mas = foldl (\as -> M.unionWith intersect as . allergens) M.empty rs
          cleanup :: [Allergen] -> M.Map Allergen [Ingredient] -> M.Map Allergen Ingredient
          cleanup [] mas' = M.map head mas'
          cleanup (a: as) mas' = case a `M.lookup` mas' of
                                   Just [i] -> cleanup as (M.mapWithKey (\a' is -> if a' == a then is else filter (/=i) is) mas')
                                   _ -> cleanup (as ++ [a]) mas'


input :: IO [Recipe]
input = map read . filter (not . null) . lines <$> readFile "input"

part1solution :: IO ()
part1solution = do
    rs <- input
    let si = S.fromList . M.elems . revealAllergens $ rs
    print . length . concatMap (\(Recipe is _) -> filter (`S.notMember` si) is) $ rs

part2solution :: IO ()
part2solution = print . intercalate "," . map (\(Ingredient cs) -> cs) . M.elems . revealAllergens =<< input
