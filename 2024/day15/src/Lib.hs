module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Set as S

data Point = P
  { px :: Int
  , py :: Int
  } deriving (Eq, Ord, Show)

pointToGPS :: Point -> Int
pointToGPS p = px p + 100 * py p

data Dir
  = U
  | R
  | L
  | D
  deriving (Eq, Show)

charToDir :: Char -> Dir
charToDir '^' = U
charToDir '>' = R
charToDir 'v' = D
charToDir '<' = L
charToDir _ = U

move :: Dir -> Int -> Point -> Point
move U n p = p {py = py p - n}
move R n p = p {px = px p + n}
move D n p = p {py = py p + n}
move L n p = p {px = px p - n}

data WH = WH
  { whWalls :: S.Set Point
  , whBoxes :: S.Set Point
  , whR :: Point
  , whMoves :: [Dir]
  }

instance Show WH where
  show wh =
    intercalate
      "\n"
      [ [ if isWall
        then '#'
        else if isBox
               then 'O'
               else if isRobot
                      then '@'
                      else '.'
      | x <- [0 .. maxX]
      , let p = P x y
      , let isWall = p `S.member` whWalls wh
      , let isBox = p `S.member` whBoxes wh
      , let isRobot = p == whR wh
      ]
      | y <- [0 .. maxY]
      ]
    where
      maxX = S.findMax . S.map px . whWalls $ wh
      maxY = S.findMax . S.map py . whWalls $ wh

parseInput :: String -> WH
parseInput cs = WH walls boxes robot mvs
  where
    css = splitOn "\n\n" cs
    mps = lines . head $ css
    mpW = length . head $ mps
    mpH = length mps
    walls =
      S.fromList
        [ p
        | x <- [0 .. mpW - 1]
        , y <- [0 .. mpH - 1]
        , let p = P x y
        , mps !! y !! x == '#'
        ]
    boxes =
      S.fromList
        [ p
        | x <- [0 .. mpW - 1]
        , y <- [0 .. mpH - 1]
        , let p = P x y
        , mps !! y !! x == 'O'
        ]
    robot =
      head
        [ p
        | x <- [0 .. mpW - 1]
        , y <- [0 .. mpH - 1]
        , let p = P x y
        , mps !! y !! x == '@'
        ]
    mvs = fmap charToDir . filter (/= '\n') . last $ css

goRobot :: (WH -> WH) -> WH -> WH
goRobot makeStep = head . dropWhile (not . null . whMoves) . iterate makeStep

makeStep1 :: WH -> WH
makeStep1 wh
  | (null . whMoves) wh = wh
  | isBlocked = wh {whMoves = tail . whMoves $ wh}
  | otherwise =
    wh
      { whMoves = tail . whMoves $ wh
      , whBoxes =
          S.map
            (\p ->
               if p `S.member` boxesToMove
                 then move d 1 p
                 else p)
            . whBoxes
            $ wh
      , whR = move d 1 . whR $ wh
      }
  where
    d = head . whMoves $ wh
    boxesToMove =
      S.fromList
        . takeWhile (`S.member` whBoxes wh)
        . iterate (move d 1)
        . move d 1
        . whR
        $ wh
    isBlocked = (move d (S.size boxesToMove + 1) . whR) wh `S.member` whWalls wh

part1Solution :: String -> Int
part1Solution =
  sum . fmap pointToGPS . S.toList . whBoxes . goRobot makeStep1 . parseInput

doublePoint :: Point -> Point
doublePoint p = p {px = 2 * px p}

doubleSize :: WH -> WH
doubleSize wh =
  wh
    { whWalls = S.map doublePoint . whWalls $ wh
    , whBoxes = S.map doublePoint . whBoxes $ wh
    , whR = doublePoint . whR $ wh
    }

makeStep2 :: WH -> WH
makeStep2 wh
  | (null . whMoves) wh = wh
  | (d == L || d == R) && isBlockedHor = wh {whMoves = tail . whMoves $ wh}
  | d == L || d == R =
    wh
      { whMoves = tail . whMoves $ wh
      , whBoxes =
          S.map
            (\p ->
               if p `S.member` boxesToMoveHor
                 then move d 1 p
                 else p)
            . whBoxes
            $ wh
      , whR = rp
      }
  | isBlockedVer = wh {whMoves = tail . whMoves $ wh}
  | otherwise =
    wh
      { whMoves = tail . whMoves $ wh
      , whBoxes =
          S.map
            (\p ->
               if p `S.member` boxesToMoveVerS
                 then move d 1 p
                 else p)
            . whBoxes
            $ wh
      , whR = move d 1 . whR $ wh
      }
  where
    d = head . whMoves $ wh
    boxesToMoveHor =
      S.fromList
        . takeWhile (`S.member` whBoxes wh)
        . iterate (move d 2)
        . move
            d
            (if d == L
               then 2
               else 1)
        . whR
        $ wh
    isBlockedHor =
      (move
         d
         (S.size boxesToMoveHor * 2
            + (if d == L
                 then 2
                 else 1))
         . whR)
        wh
        `S.member` whWalls wh
    getBoxesToMoveVer :: Bool -> [Point] -> [Point]
    getBoxesToMoveVer _ [] = []
    getBoxesToMoveVer isRobot ps = boxRow <> getBoxesToMoveVer False boxRow
      where
        boxRow =
          concatMap
            (\p ->
               let p' = move d 1 p
                   p'' = move R 1 p'
                   p''' = move L 1 p'
                in [p' | p' `S.member` whBoxes wh]
                     <> [p'' | not isRobot && p'' `S.member` whBoxes wh]
                     <> [p''' | p''' `S.member` whBoxes wh])
            ps
    boxesToMoveVer = getBoxesToMoveVer True [whR wh]
    boxesToMoveVerS = S.fromList boxesToMoveVer
    rp = move d 1 . whR $ wh
    isBlockedVer =
      rp `S.member` whWalls wh
        || move L 1 rp `S.member` whWalls wh
        || any
             (\p ->
                let p' = move d 1 p
                    p'' = move R 1 p'
                    p''' = move L 1 p'
                 in p' `S.member` whWalls wh
                      || p'' `S.member` whWalls wh
                      || p''' `S.member` whWalls wh)
             boxesToMoveVer

part2Solution :: String -> Int
part2Solution =
  sum
    . fmap pointToGPS
    . S.toList
    . whBoxes
    . goRobot makeStep2
    . doubleSize
    . parseInput
