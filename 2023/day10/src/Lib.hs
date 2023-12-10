module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S

data Point = P {px :: Int, py :: Int} deriving (Show, Eq, Ord)

data Labyrinth = Labyrinth
  { lWidth :: Int,
    lHeight :: Int,
    lAnimal :: Point,
    lPipes :: M.Map Point Char
  }
  deriving (Show)

connects :: Point -> Char -> [Point]
connects (P x y) '|' = [P x (y - 1), P x (y + 1)]
connects (P x y) '-' = [P (x - 1) y, P (x + 1) y]
connects (P x y) 'L' = [P x (y - 1), P (x + 1) y]
connects (P x y) 'J' = [P x (y - 1), P (x - 1) y]
connects (P x y) '7' = [P (x - 1) y, P x (y + 1)]
connects (P x y) 'F' = [P (x + 1) y, P x (y + 1)]
connects _ _ = []

allPipes :: String
allPipes = "|-LJ7F"

canGo :: Labyrinth -> Point -> [Point]
canGo l p = case p `M.lookup` lPipes l of
  Just c ->
    filter
      ( \p' -> case p' `M.lookup` lPipes l of
          Just c' -> (elem p . connects p') c'
          _ -> False
      )
      . connects p
      $ c
  _ -> []

mkLabyrinth :: [String] -> Labyrinth
mkLabyrinth css =
  Labyrinth
    { lWidth = w,
      lHeight = h,
      lPipes = pipes,
      lAnimal = animal
    }
  where
    w = length . head $ css
    h = length css
    pipesAndCoords = concatMap (\(y, cs) -> zipWith (\x c -> (P x y, c)) [0 ..] cs) . zip [0 ..] $ css
    animal = fst . head . filter ((== 'S') . snd) $ pipesAndCoords
    pipes = M.fromList . filter ((/= '.') . snd) $ pipesAndCoords

mostDistantAndPath :: Labyrinth -> Maybe (Int, S.Set Point)
mostDistantAndPath l = doer (S.singleton animal) (S.singleton animal) 0
  where
    animal = lAnimal l

    doer :: S.Set Point -> S.Set Point -> Int -> Maybe (Int, S.Set Point)
    doer front visited step
      | null front = Nothing
      | (not . null) ends = Just (step, visited')
      | otherwise = doer front' visited' (step + 1)
      where
        front' = S.fromList . concatMap (filter (`S.notMember` visited) . canGo l) . S.toList $ front
        visited' = visited <> front'

        isEnd :: Point -> Bool
        isEnd p = length ps == 2 && all (`S.member` visited) ps
          where
            ps = canGo l p

        ends = filter isEnd . S.toList $ front

setAnimal :: Labyrinth -> Char -> Labyrinth
setAnimal l c = l {lPipes = M.insert (lAnimal l) c (lPipes l)}

part1Solution :: [String] -> Int
part1Solution = maximum . (\l -> fmap (maybe (-1) fst . mostDistantAndPath . setAnimal l) allPipes) . mkLabyrinth

part2Solution :: [String] -> Int
part2Solution css = length . filter isInside $ [P x y | x <- [0 .. maxX], y <- [0 .. maxY]]
  where
    l = mkLabyrinth css
    maxX = lWidth l - 1
    maxY = lHeight l - 1
    (goodLabyrinth, path) =
      fromMaybe (l, S.empty)
        . head
        . filter isJust
        . (\l' -> fmap (\c -> let l'' = setAnimal l' c in fmap (\(_, path') -> (l'', path')) . mostDistantAndPath $ l'') allPipes)
        $ l

    isInside :: Point -> Bool
    isInside p =
      p `S.notMember` path
        && isOddH [('7', 'L'), ('J', 'F')] [p {px = x} | x <- [px p - 1, px p - 2 .. 0]]
        && isOddH [('L', '7'), ('F', 'J')] [p {px = x} | x <- [px p + 1 .. maxX]]
        && isOddV [('L', '7'), ('J', 'F')] [p {py = y} | y <- [py p - 1, py p - 2 .. 0]]
        && isOddV [('7', 'L'), ('F', 'J')] [p {py = y} | y <- [py p + 1 .. maxY]]
      where
        isOddH = isOdd '-'
        isOddV = isOdd '|'
        isOdd :: Char -> [(Char, Char)] -> [Point] -> Bool
        isOdd cExclude asOnes ps = odd . length $ row
          where
            row =
              (`fixAsOnes` asOnes)
                . filter (/= cExclude)
                . fmap (fromMaybe '.' . (`M.lookup` lPipes goodLabyrinth))
                . filter (`S.member` path)
                $ ps

        fixAsOnes :: String -> [(Char, Char)] -> String
        fixAsOnes [] _ = []
        fixAsOnes cs@[_] _ = cs
        fixAsOnes (c1 : c2 : cs) asOnes
          | (c1, c2) `elem` asOnes = '+' : fixAsOnes cs asOnes
          | otherwise = c1 : fixAsOnes (c2 : cs) asOnes
