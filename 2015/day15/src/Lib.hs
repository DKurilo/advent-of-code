module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

type Capacity = Int

type Durability = Int

type Flavor = Int

type Texture = Int

type Calories = Int

data Ingredient = Ingredient String Capacity Durability Flavor Texture Calories deriving (Show)

readProperty :: String -> P.ReadP Int
readProperty cs = do
  P.string cs
  P.char ' '
  PC.readPrec_to_P readPrec 0

instance Read Ingredient where
  readPrec = do
    name <- (lift . P.munch1) isAlpha
    (lift . P.string) ": "
    capacity <- (lift . readProperty) "capacity"
    (lift . P.string) ", "
    durability <- (lift . readProperty) "durability"
    (lift . P.string) ", "
    flavor <- (lift . readProperty) "flavor"
    (lift . P.string) ", "
    texture <- (lift . readProperty) "texture"
    (lift . P.string) ", "
    calories <- (lift . readProperty) "calories"
    return $ Ingredient name capacity durability flavor texture calories

allRecipes :: [Ingredient] -> [[(Ingredient, Int)]]
allRecipes = doer 100
  where
    doer _ [] = []
    doer 0 _ = [[]]
    doer n [i] = [[(i, n)]]
    doer n (i : is) = concatMap (\k -> map ((i, k) :) . doer (n - k) $ is) [0 .. n]

score :: [(Ingredient, Int)] -> Int
score = sumScore . foldl addIngredient (0, 0, 0, 0)
  where
    addIngredient (cp, dr, fl, tx) (Ingredient _ cp' dr' fl' tx' _, n) = (cp + n * cp', dr + dr' * n, fl + fl' * n, tx + tx' * n)
    sumScore (cp, dr, fl, tx) = bound cp * bound dr * bound fl * bound tx
    bound n
      | n < 0 = 0
      | otherwise = n

calories :: [(Ingredient, Int)] -> Int
calories = foldl (\cl (Ingredient _ _ _ _ _ cl', n) -> cl + cl' * n) 0

part1Solution :: [Ingredient] -> Int
part1Solution = maximum . map score . allRecipes

part2Solution :: [Ingredient] -> Int
part2Solution = maximum . map score . filter ((== 500) . calories) . allRecipes
