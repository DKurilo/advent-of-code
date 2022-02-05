module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', nub, sort)
import qualified Data.Vector as V
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Direction = U | D | R | L deriving (Eq, Ord, Show)

data Cart = Cart {cY :: Int, cX :: Int, cD :: Direction, cMem :: Int} deriving (Eq, Ord, Show)

data TandC = TandC {tcTracks :: V.Vector (V.Vector Char), tcCarts :: [Cart]} deriving (Eq, Ord, Show)

parseTrack :: Char -> Char
parseTrack '|' = '|'
parseTrack ' ' = ' '
parseTrack '-' = '-'
parseTrack '+' = '+'
parseTrack '/' = '/'
parseTrack '\\' = '\\'
parseTrack '>' = '-'
parseTrack '<' = '-'
parseTrack 'v' = '|'
parseTrack '^' = '|'
parseTrack _ = ' '

isCart :: Char -> Bool
isCart '>' = True
isCart '<' = True
isCart '^' = True
isCart 'v' = True
isCart _ = False

parseDirection :: Char -> Direction
parseDirection '>' = R
parseDirection '<' = L
parseDirection '^' = U
parseDirection 'v' = D

parseTandC :: [String] -> TandC
parseTandC css = TandC tracks carts
  where
    tracks = V.fromList [V.fromList [parseTrack (css !! y !! x) | x <- [0 .. (length . (!! y)) css - 1]] | y <- [0 .. length css - 1]]
    carts =
      sort
        [ Cart y x (parseDirection c) 0
          | y <- [0 .. length css - 1],
            x <- [0 .. (length . (!! y)) css - 1],
            let c = css !! y !! x,
            isCart c
        ]

turnCart :: Direction -> Int -> Direction
turnCart R 0 = U
turnCart L 0 = D
turnCart D 0 = R
turnCart U 0 = L
turnCart d 1 = d
turnCart R 2 = D
turnCart L 2 = U
turnCart D 2 = L
turnCart U 2 = R
turnCart d _ = d

moveCart :: Cart -> V.Vector (V.Vector Char) -> Cart
moveCart c tracks
  | d == R && tR == '-' = c {cX = cX c + 1}
  | d == R && tR == '/' = c {cX = cX c + 1, cD = U}
  | d == R && tR == '\\' = c {cX = cX c + 1, cD = D}
  | d == R && tR == '+' = c {cX = cX c + 1, cD = turnCart (cD c) (cMem c), cMem = (cMem c + 1) `mod` 3}
  | d == L && tL == '-' = c {cX = cX c - 1}
  | d == L && tL == '/' = c {cX = cX c - 1, cD = D}
  | d == L && tL == '\\' = c {cX = cX c - 1, cD = U}
  | d == L && tL == '+' = c {cX = cX c - 1, cD = turnCart (cD c) (cMem c), cMem = (cMem c + 1) `mod` 3}
  | d == D && tD == '|' = c {cY = cY c + 1}
  | d == D && tD == '/' = c {cY = cY c + 1, cD = L}
  | d == D && tD == '\\' = c {cY = cY c + 1, cD = R}
  | d == D && tD == '+' = c {cY = cY c + 1, cD = turnCart (cD c) (cMem c), cMem = (cMem c + 1) `mod` 3}
  | d == U && tU == '|' = c {cY = cY c - 1}
  | d == U && tU == '/' = c {cY = cY c - 1, cD = R}
  | d == U && tU == '\\' = c {cY = cY c - 1, cD = L}
  | d == U && tU == '+' = c {cY = cY c - 1, cD = turnCart (cD c) (cMem c), cMem = (cMem c + 1) `mod` 3}
  | otherwise = c
  where
    d = cD c
    tR = tracks V.! cY c V.! (cX c + 1)
    tL = tracks V.! cY c V.! (cX c - 1)
    tU = tracks V.! (cY c - 1) V.! cX c
    tD = tracks V.! (cY c + 1) V.! cX c

tick :: TandC -> (TandC, [(Int, Int)])
tick tc = (tc {tcCarts = sort carts}, (sort . nub) collisions)
  where
    (carts, collisions) = foldl' doer ([], []) . tcCarts $ tc
    doer :: ([Cart], [(Int, Int)]) -> Cart -> ([Cart], [(Int, Int)])
    doer (carts', collisions') c
      | (cY c, cX c) `elem` collisions' = (carts', collisions')
      | null collisions'' = (c' : carts', collisions')
      | otherwise = (cartsNoBroken, collisions'' ++ collisions')
      where
        c' = moveCart c (tcTracks tc)
        findCollision = map (\c'' -> (cY c'', cX c'')) . filter (\c'' -> cX c' == cX c'' && cY c' == cY c'')
        collisions'' = (if cD c == R || cD c == D then (findCollision . tcCarts) tc else []) ++ findCollision carts'
        cartsNoBroken = filter (\c -> (cY c, cX c) `notElem` collisions'') carts'

moveUntilCollision :: TandC -> (TandC, [(Int, Int)])
moveUntilCollision t
  | (null . tcCarts) t' = (t', cs)
  | null cs = moveUntilCollision t'
  | otherwise = (t', cs)
  where
    (t', cs) = tick t

moveUntilLast :: TandC -> (TandC, [(Int, Int)])
moveUntilLast t
  | (length . tcCarts) t' == 1 = (t', [((\c -> (cY c, cX c)) . head . tcCarts) t'])
  | (null . tcCarts) t' = (t', [])
  | otherwise = moveUntilLast t'
  where
    (t', cs) = tick t

part1Solution :: [String] -> [(Int, Int)]
part1Solution = map (\(y, x) -> (x, y)) . snd . moveUntilCollision . parseTandC

part2Solution :: [String] -> [(Int, Int)]
part2Solution = map (\(y, x) -> (x, y)) . snd . moveUntilLast . parseTandC
