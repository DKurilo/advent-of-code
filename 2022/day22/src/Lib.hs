module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.List (transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map as M
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

rotateLeft :: [[a]] -> [[a]]
rotateLeft = reverse . transpose

rotateRight :: [[a]] -> [[a]]
rotateRight = transpose . reverse

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Show)

instance Ord Point where
  p1 <= p2 = (pY p1, pX p1) <= (pY p2, pX p2)

newtype Board = Board {unBoard :: M.Map Position (Point, Direction)} deriving (Show)

type Mapper = Point -> Direction -> (Point, Direction)

nextPoint :: [String] -> Mapper
nextPoint css (Point i j) DRight
  | i < length (css !! j) - 1 = (Point (i + 1) j, DRight)
  | otherwise = (Point (fst . head . dropWhile ((== ' ') . snd) . zip [0 ..] . (!! j) $ css) j, DRight)
nextPoint css (Point i j) DDown
  | j < length css - 1 && i < length (css !! (j + 1)) && css !! (j + 1) !! i /= ' ' = (Point i (j + 1), DDown)
  | otherwise = (Point i (fst . head . dropWhile (\(_, cs) -> i >= length cs || cs !! i == ' ') . zip [0 ..] $ css), DDown)
nextPoint css (Point i j) DLeft
  | i > 0 && css !! j !! (i - 1) /= ' ' = (Point (i - 1) j, DLeft)
  | otherwise = (Point (length (css !! j) - 1) j, DLeft)
nextPoint css (Point i j) DUp
  | j > 0 && i < length (css !! (j - 1)) && css !! (j - 1) !! i /= ' ' = (Point i (j - 1), DUp)
  | otherwise = (Point i (fst . head . dropWhile (\(_, cs) -> i >= length cs || cs !! i == ' ') . reverse . zip [0 ..] $ css), DUp)

nextPointInCube :: [String] -> Mapper
nextPointInCube css = doer
  where
    cubeSize = maximum (length css : map length css) `div` 4
    rotated = length css `div` cubeSize == 3
    sidesFlatUnfixed =
      map (map transpose . chunksOf cubeSize . transpose)
        . chunksOf cubeSize
        . (\xs -> if rotated then map (map (\((p', r), c) -> ((p', r - 1), c))) . rotateLeft $ xs else xs)
        . zipWith (\j' -> zipWith (\i' c -> ((Point i' j', 0), c)) [0 ..]) [0 ..]
        . map (\cs -> take ((if rotated then 4 else 3) * cubeSize) (cs <> repeat ' '))
        $ css
    sidesFlat =
      [ if (snd . head . head . (!! 1)) row == ' '
          then
            if (snd . head . head . head) row /= ' ' && j' == 3 || (snd . head . head . (!! 2)) row /= ' ' && j' == 0
              then
                [ if i' == 1
                    then map (map (\((p', r), c) -> ((p', r - 1), c))) . rotateLeft . head $ row
                    else
                      if i' == 0
                        then row !! 1
                        else row !! i'
                  | i' <- [0 .. 2]
                ]
              else
                if (snd . head . head . head) row /= ' ' && j' == 0 || (snd . head . head . (!! 2)) row /= ' ' && j' == 3
                  then
                    [ if i' == 1
                        then map (map (\((p', r), c) -> ((p', r + 1), c))) . rotateRight . (!! 2) $ row
                        else
                          if i' == 2
                            then row !! 1
                            else row !! i'
                      | i' <- [0 .. 2]
                    ]
                  else row
          else row
        | j' <- [0 .. 3],
          let row = sidesFlatUnfixed !! j'
      ]
    side1Index = length . takeWhile (\side -> (snd . head . head) side == ' ') $ [head (sidesFlat !! k) | k <- [0 .. 3]]
    side1 = (!! side1Index) . iterate (map (map (\((p', r), c) -> ((p', r + 1), c))) . rotateRight) . head . (!! side1Index) $ sidesFlat
    side2 = head sidesFlat !! 1
    side3Index = length . takeWhile (\side -> (snd . head . head) side == ' ') $ [sidesFlat !! k !! 2 | k <- [0 .. 3]]
    side3 = (!! side3Index) . iterate (map (map (\((p', r), c) -> ((p', r - 1), c))) . rotateLeft) . (!! 2) . (!! side3Index) $ sidesFlat
    side4 = sidesFlat !! 1 !! 1
    side5 = sidesFlat !! 2 !! 1
    side6 = sidesFlat !! 3 !! 1
    sides = [side1, side2, side3, side4, side5, side6]
    nextToSide :: Int -> Direction -> [[((Point, Int), Char)]]
    nextToSide 1 DRight = side2
    nextToSide 1 DDown = map (map (\((p', r), c) -> ((p', r + 1), c))) . rotateRight $ side4
    nextToSide 1 DLeft = map (map (\((p', r), c) -> ((p', r + 2), c))) . rotateRight . rotateRight $ side5
    nextToSide 1 DUp = map (map (\((p', r), c) -> ((p', r - 1), c))) . rotateLeft $ side6
    nextToSide 2 DRight = side3
    nextToSide 2 DDown = side4
    nextToSide 2 DLeft = side1
    nextToSide 2 DUp = side6
    nextToSide 3 DRight = map (map (\((p', r), c) -> ((p', r + 2), c))) . rotateRight . rotateRight $ side5
    nextToSide 3 DDown = map (map (\((p', r), c) -> ((p', r - 1), c))) . rotateLeft $ side4
    nextToSide 3 DLeft = side2
    nextToSide 3 DUp = map (map (\((p', r), c) -> ((p', r + 1), c))) . rotateRight $ side6
    nextToSide 4 DRight = map (map (\((p', r), c) -> ((p', r + 1), c))) . rotateRight $ side3
    nextToSide 4 DDown = side5
    nextToSide 4 DLeft = map (map (\((p', r), c) -> ((p', r - 1), c))) . rotateLeft $ side1
    nextToSide 4 DUp = side2
    nextToSide 5 DRight = map (map (\((p', r), c) -> ((p', r + 2), c))) . rotateRight . rotateRight $ side3
    nextToSide 5 DDown = side6
    nextToSide 5 DLeft = map (map (\((p', r), c) -> ((p', r + 2), c))) . rotateRight . rotateRight $ side1
    nextToSide 5 DUp = side4
    nextToSide 6 DRight = map (map (\((p', r), c) -> ((p', r - 1), c))) . rotateLeft $ side3
    nextToSide 6 DDown = side2
    nextToSide 6 DLeft = map (map (\((p', r), c) -> ((p', r + 1), c))) . rotateRight $ side1
    nextToSide 6 DUp = side5
    nextToSide _ _ = error "wrong side"
    doer p d = pos'
      where
        sideIndex = fst . head . filter (elem p . snd) . zip [0 ..] . map (map (fst . fst) . concat) $ sides
        pOnSide = head [Point i' j' | j' <- [0 .. cubeSize - 1], i' <- [0 .. cubeSize - 1], p == (fst . fst) (sides !! sideIndex !! j' !! i')]
        d' = iterate nextDirection d !! ((snd . fst $ sides !! sideIndex !! pY pOnSide !! pX pOnSide) `mod` 4)
        side' = case d' of
          DRight -> zipWith (<>) (sides !! sideIndex) (nextToSide (sideIndex + 1) d')
          DDown -> (sides !! sideIndex) <> nextToSide (sideIndex + 1) d'
          DLeft -> zipWith (<>) (nextToSide (sideIndex + 1) d') (sides !! sideIndex)
          DUp -> nextToSide (sideIndex + 1) d' <> (sides !! sideIndex)
        pOnSide' = case d' of
          DRight -> pOnSide {pX = pX pOnSide + 1}
          DDown -> pOnSide {pY = pY pOnSide + 1}
          DLeft -> pOnSide {pX = pX pOnSide - 1 + cubeSize}
          DUp -> pOnSide {pY = pY pOnSide - 1 + cubeSize}
        d'' = iterate nextDirection d' !! (((0 -) . snd . fst $ side' !! pY pOnSide' !! pX pOnSide') `mod` 4)
        pos' = (fst . fst $ side' !! pY pOnSide' !! pX pOnSide', d'')

pointsAround :: Mapper -> Point -> [String] -> [(Direction, (Point, Direction))]
pointsAround mapper p css =
  [ p'
    | p' <- map (\d -> (d, mapper p d)) [DRight, DDown, DLeft, DUp],
      css !! (pY . fst . snd) p' !! (pX . fst . snd) p' == '.'
  ]

parseBoard :: Mapper -> [String] -> Board
parseBoard mapper css =
  Board . M.fromList . concat $
    [ map (first (Pos p)) . pointsAround mapper p $ css
      | j <- [0 .. length css - 1],
        i <- [0 .. length (css !! j) - 1],
        let p = Point i j,
        j < length css,
        j >= 0,
        i < length (css !! j),
        i >= 0,
        css !! j !! i == '.'
    ]

splitInput :: [String] -> ([String], String)
splitInput css = (take (length css - 2) css, last css)

data Move = R | L | F Int deriving (Show)

instance Read Move where
  readPrec =
    ( do
        _ <- lift . P.char $ 'R'
        return R
    )
      PC.<++ ( do
                 _ <- lift . P.char $ 'L'
                 return L
             )
      PC.<++ ( do
                 F <$> readPrec
             )

newtype Path = Path {unPath :: [Move]} deriving (Show)

instance Read Path where
  readPrec = fmap Path . lift . P.many1 . PC.readPrec_to_P readPrec $ 0

data Direction = DRight | DLeft | DUp | DDown deriving (Eq, Ord, Show)

nextDirection :: Direction -> Direction
nextDirection DRight = DDown
nextDirection DDown = DLeft
nextDirection DLeft = DUp
nextDirection DUp = DRight

scoreDirection :: Direction -> Int
scoreDirection DRight = 0
scoreDirection DDown = 1
scoreDirection DLeft = 2
scoreDirection DUp = 3

data Position = Pos {posPoint :: Point, posDirection :: Direction} deriving (Eq, Ord, Show)

turn :: Position -> Move -> Position
turn pos (F _) = pos
turn (Pos p DRight) L = Pos p DUp
turn (Pos p DDown) L = Pos p DRight
turn (Pos p DLeft) L = Pos p DDown
turn (Pos p DUp) L = Pos p DLeft
turn (Pos p DRight) R = Pos p DDown
turn (Pos p DDown) R = Pos p DLeft
turn (Pos p DLeft) R = Pos p DUp
turn (Pos p DUp) R = Pos p DRight

goForward :: Int -> Position -> Board -> Position
goForward n pos board@(Board tiles)
  | n == 0 = pos
  | otherwise = goForward (n - 1) pos' board
  where
    pos' = case pos `M.lookup` tiles of
      Just p' -> uncurry Pos p'
      _ -> pos

scorePosition :: Position -> Int
scorePosition (Pos p d) = (pY p + 1) * 1000 + (pX p + 1) * 4 + scoreDirection d

followPath :: Path -> Board -> Position
followPath path board = doer (unPath path) (Pos startPoint DRight)
  where
    startPoint = minimum . map posPoint . M.keys . unBoard $ board
    doer :: [Move] -> Position -> Position
    doer [] pos = pos
    doer (F n : ms) pos = doer ms (goForward n pos board)
    doer (m : ms) pos = doer ms (turn pos m)

pathScore :: ([String] -> Mapper) -> [String] -> Int
pathScore mapper css = scorePosition $ followPath path . parseBoard (mapper rawBoard) $ rawBoard
  where
    (rawBoard, rawPath) = splitInput css
    path = read rawPath :: Path

part1Solution :: [String] -> Int
part1Solution = pathScore nextPoint

part2Solution :: [String] -> Int
part2Solution = pathScore nextPointInCube
