module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (second)
import Data.Function (on)
import Data.List (find, foldl', intercalate, nub, sort, sortBy)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (Ordering (..))
import qualified Data.Vector as V
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Space = W | E deriving (Eq, Show)

parseSpace :: Char -> Space
parseSpace '#' = W
parseSpace '.' = E
parseSpace 'E' = E
parseSpace 'G' = E
parseSpace _ = W

data UnitType = Elf | Goblin deriving (Eq, Ord, Show)

parseUnitType :: Char -> Maybe UnitType
parseUnitType 'E' = Just Elf
parseUnitType 'G' = Just Goblin
parseUnitType _ = Nothing

enemy :: UnitType -> UnitType
enemy Elf = Goblin
enemy Goblin = Elf

data Point = Point {pY :: Int, pX :: Int} deriving (Eq, Ord, Show)

dist :: Point -> Point -> Int
dist p1 p2 = abs (pX p1 - pX p2) + abs (pY p1 - pY p2)

data Unit = Unit {uP :: Point, uHealth :: Int, uAc :: Int, uType :: UnitType} deriving (Eq, Ord, Show)

mkUnit :: Int -> Int -> UnitType -> Unit
mkUnit x y = Unit (Point y x) 200 3

data Battle = Battle {bMap :: V.Vector (V.Vector Space), bUnits :: [Unit]} deriving (Show)

showBattle :: Battle -> String
showBattle b = intercalate "\n" $ do
  y <- [0 .. (V.length . bMap) b - 1]
  return
    ( do
        x <- [0 .. V.length (bMap b V.! y) - 1]
        return $ case (find (\u -> uP u == Point y x) . bUnits) b of
          Just u
            | uType u == Elf -> 'E'
            | uType u == Goblin -> 'G'
          _
            | bMap b V.! y V.! x == W -> '#'
            | otherwise -> '.'
    )

parseBattle :: [String] -> Battle
parseBattle css = Battle bmap units
  where
    l = length css
    bmap = V.fromList [V.fromList [parseSpace (css !! y !! x) | x <- [0 .. length (css !! y) - 1]] | y <- [0 .. l - 1]]
    units =
      [ mkUnit x y ut
        | y <- [0 .. l - 1],
          x <- [0 .. length (css !! y) - 1],
          let mbut = parseUnitType (css !! y !! x),
          isJust mbut,
          let (Just ut) = mbut
      ]

occupied :: Battle -> [Point]
occupied = map uP . bUnits

targets :: UnitType -> Battle -> [Unit]
targets ut = filter ((== ut) . uType) . bUnits

inRanges :: UnitType -> Battle -> [Point]
inRanges ut b =
  sort
    . filter (\p -> bMap b V.! pY p V.! pX p == E && p `notElem` occs)
    . nub
    . concatMap (\u -> [Point ((pY . uP) u + dy) ((pX . uP) u + dx) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]])
    . targets ut
    $ b
  where
    occs = occupied b

ways :: Point -> Point -> Battle -> [(Point, Int)]
ways p1 p2 b
  | p1 == p2 = [(p1, 0)]
  | null ws = []
  | otherwise = [minimum . filter ((== minWayLength) . snd) $ ws]
  where
    occs = occupied b

    isEmpty p = bMap b V.! pY p V.! pX p == E && p `notElem` occs

    mkDistMx :: M.Map Point Int -> [Point] -> Int -> M.Map Point Int
    mkDistMx visited [] _ = visited
    mkDistMx visited front step
      | p2 `M.member` visited = visited
      | otherwise = mkDistMx visited' front' (step + 1)
      where
        (visited', front') = foldl' doer (visited, []) front

        doer :: (M.Map Point Int, [Point]) -> Point -> (M.Map Point Int, [Point])
        doer (vs, fs) p = foldl' (\(vs', fs') p' -> (M.insert p' step vs', p' : fs')) (vs, fs) ps
          where
            ps =
              [ p'
                | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)],
                  let p' = Point (pY p + dy) (pX p + dx),
                  p' `M.notMember` vs,
                  p' `notElem` fs,
                  isEmpty p'
              ]

    ws =
      map (second (fromMaybe 0 . M.lookup p2))
        . filter
          (M.member p2 . snd)
        $ [ (p', mkDistMx (M.singleton p' 1) [p'] 2)
            | (dy, dx) <- [(-1, 0), (0, -1), (0, 1), (1, 0)],
              let p' = Point (pY p1 + dy) (pX p1 + dx),
              isEmpty p'
          ]
    minWayLength = minimum . map snd $ ws

moveUnit :: Unit -> Battle -> Maybe Unit
moveUnit u b
  | null nextPs = Nothing
  | otherwise = listToMaybe . sort . map (\(p, _) -> u {uP = p}) . filter ((== minPathLength) . snd) $ nextPs
  where
    nextPs =
      concatMap (\p -> ways (uP u) p b)
        . inRanges (enemy . uType $ u)
        $ b
    minPathLength = minimum . map snd $ nextPs

compareTargets :: Unit -> Unit -> Ordering
compareTargets u1 u2 = case uHealth u1 `compare` uHealth u2 of
  LT -> LT
  GT -> GT
  EQ -> u1 `compare` u2

attack :: Unit -> Battle -> Battle
attack u b
  | isJust mbTarget && uHealth t' > 0 = b {bUnits = sort (t' : (filter (/= t) . bUnits) b)}
  | isJust mbTarget && uHealth t' <= 0 = b {bUnits = sort ((filter (/= t) . bUnits) b)}
  | otherwise = b
  where
    mbTarget = listToMaybe . sortBy compareTargets $ [u' | u' <- targets ((enemy . uType) u) b, dist (uP u) (uP u') == 1]
    (Just t) = mbTarget
    t' = t {uHealth = uHealth t - uAc u}

turn :: Point -> Battle -> Maybe Battle
turn p b
  | null ts = Nothing
  | null us = Just b -- it's dead
  | p `elem` rs = Just (attack u b)
  | isJust mbu' = Just (attack u' (b' {bUnits = sort (u' : bUnits b')}))
  | otherwise = Just (attack u b)
  where
    ts = targets enType b
    us = filter ((== p) . uP) . bUnits $ b
    u = if null us then mkUnit 0 0 Goblin else head us -- something is completely wrong here! I don't know what exactly yet
    enType = enemy . uType $ u
    mbu' = moveUnit u b'
    (Just u') = mbu'
    b' = b {bUnits = filter (/= u) . bUnits $ b}
    rs = inRanges enType b'

battleRound :: Battle -> (Battle, Bool)
battleRound b = foldl' doer (b, True) . map uP . bUnits $ b
  where
    doer :: (Battle, Bool) -> Point -> (Battle, Bool)
    doer r@(b', False) _ = r
    doer (b', True) p = case turn p b' of
      Just b'' -> (b'', True)
      _ -> (b', False)

fight :: Battle -> (Battle, Int)
fight b
  | notFinished = (b'', n + 1)
  | otherwise = (b', 0)
  where
    (b', notFinished) = battleRound b
    (b'', n) = fight b'

part1Solution :: [String] -> Int
part1Solution css = n * (sum . map uHealth . bUnits) b
  where
    (b, n) = fight . parseBattle $ css

imporoveElves :: Int -> Battle -> Battle
imporoveElves ac b = b {bUnits = map (\u -> if uType u == Elf then u {uAc = ac} else u) . bUnits $ b}

minAc = 4

maxAc = 80 -- Elf can stay 200 / 12 rounds in case 4 goblins are attacking.
-- So 80 (even less than) is enough to kill all Goblins around

isGoodBattle :: Battle -> Battle -> Bool
isGoodBattle bStart bEnd = (not . any ((== Goblin) . uType) . bUnits) bEnd && elvesCount bStart == elvesCount bEnd

elvesCount :: Battle -> Int
elvesCount = length . filter ((== Elf) . uType) . bUnits

findBestAc :: Int -> Int -> Battle -> Int
findBestAc from to b
  | from == to = from
  | to - from == 1 = if checkBattle from then from else to
  | checkBattle middle = findBestAc from middle b
  | otherwise = findBestAc middle to b
  where
    middle = (from + to) `div` 2
    checkBattle n = isGoodBattle b . fst . fight . imporoveElves n $ b

part2Solution :: [String] -> Int
part2Solution css = n * (sum . map uHealth . bUnits . imporoveElves goodAc) b'
  where
    b = parseBattle css
    goodAc = findBestAc minAc maxAc b
    (b', n) = fight . imporoveElves goodAc $ b
