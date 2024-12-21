{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.Char (isDigit)
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

data Move
  = U
  | R
  | D
  | L
  | A
  deriving (Eq, Ord, Show)

data Point = P
  { px :: Int
  , py :: Int
  } deriving (Eq, Ord, Show)

move :: Point -> Move -> Point
move p U = p {py = py p + 1}
move p R = p {px = px p + 1}
move p D = p {py = py p - 1}
move p L = p {px = px p - 1}
move p A = p

moveBack :: Move -> Move
moveBack U = D
moveBack R = L
moveBack D = U
moveBack L = R
moveBack A = A

digitalKeyboard :: M.Map Point Char
digitalKeyboard =
  M.fromList
    [ (P 1 0, '0')
    , (P 2 0, 'A')
    , (P 0 1, '1')
    , (P 1 1, '2')
    , (P 2 1, '3')
    , (P 0 2, '4')
    , (P 1 2, '5')
    , (P 2 2, '6')
    , (P 0 3, '7')
    , (P 1 3, '8')
    , (P 2 3, '9')
    ]

digitalKeyboardReverse :: M.Map Char Point
digitalKeyboardReverse =
  M.fromList . fmap (\(x, y) -> (y, x)) . M.toList $ digitalKeyboard

pointForChar :: Char -> Point
pointForChar c = fromMaybe (P 0 0) . M.lookup c $ digitalKeyboardReverse

controlKeyboad :: M.Map Point Move
controlKeyboad =
  M.fromList [(P 0 0, L), (P 1 0, D), (P 2 0, R), (P 1 1, U), (P 2 1, A)]

controlKeyboadReverse :: M.Map Move Point
controlKeyboadReverse =
  M.fromList . fmap (\(x, y) -> (y, x)) . M.toList $ controlKeyboad

pointForMove :: Move -> Point
pointForMove m = fromMaybe (P 0 0) . M.lookup m $ controlKeyboadReverse

bfs :: M.Map Point a -> Point -> Point -> [[Move]]
bfs m p0 p1 =
  fmap reverse . backtrack p1 $ doer (S.singleton p0) (M.singleton p0 0) 1
  where
    doer :: S.Set Point -> M.Map Point Int -> Int -> M.Map Point Int
    doer front visited s
      | p1 `S.member` front = visited
      | S.null front = visited
      | otherwise = doer front' (M.unionWith min visited visited') (s + 1)
      where
        front' =
          S.fromList
            . concatMap
                (\p ->
                   [ p'
                   | dir <- [U, R, D, L]
                   , let p' = move p dir
                   , p' `M.member` m && p' `M.notMember` visited
                   ])
            . S.toList
            $ front
        visited' = M.fromList . fmap (, s) . S.toList $ front'
    backtrack :: Point -> M.Map Point Int -> [[Move]]
    backtrack p visited
      | p == p0 = [[]]
      | otherwise =
        concatMap (\(p'', mv) -> fmap (mv :) . backtrack p'' $ visited) pms
      where
        sForP p' = fromMaybe (-100) . M.lookup p' $ visited
        s = sForP p
        pms =
          filter (\(p', _) -> s - sForP p' == 1)
            . fmap (\(m', w) -> (move p w, m'))
            $ [(U, D), (R, L), (D, U), (L, R)]

memoControlKeybMove :: Move -> Move -> [[Move]]
memoControlKeybMove A L = [[D, L, L, A]]
memoControlKeybMove L A = [[R, R, U, A]]
memoControlKeybMove m0 m1 = fromMaybe [] . M.lookup (m0, m1) $ memo
  where
    memo =
      M.fromList
        [ ((m0', m1'), fmap (<> [A]) . bfs controlKeyboad p0 $ p1)
        | m0' <- M.elems controlKeyboad
        , let p0 = pointForMove m0'
        , m1' <- M.elems controlKeyboad
        , let p1 = pointForMove m1'
        ]

buildPathsFromParts :: [[[a]]] -> [[a]]
buildPathsFromParts [] = [[]]
buildPathsFromParts (mvs:rest) =
  concatMap (\pth -> fmap (\mv -> mv <> pth) mvs) . buildPathsFromParts $ rest

getControlSeq' :: Int -> [Move] -> [[Move]]
getControlSeq' n = (!! n) . iterate (concatMap contSeq) . (: [])
  where
    contSeq moves =
      buildPathsFromParts . fmap (uncurry memoControlKeybMove) . zip (A : moves)
        $ moves

getControlSeq :: Int -> String -> [[Move]]
getControlSeq n code =
  (!! n) . iterate (concatMap contSeq) . buildPathsFromParts $ digSeq
  where
    digSeq =
      fmap
        (\(c0, c1) ->
           let p0 = pointForChar c0
               p1 = pointForChar c1
            in fmap (<> [A]) . bfs digitalKeyboard p0 $ p1)
        . zip ('A' : code)
        $ code
    contSeq moves =
      buildPathsFromParts . fmap (uncurry memoControlKeybMove) . zip (A : moves)
        $ moves

toNumber :: String -> Int
toNumber = read . filter isDigit

part1Solution :: [String] -> Int
part1Solution =
  sum . fmap (\code -> toNumber code * (minimum . getControlSeq2 2) code)
  -- sum
  --   . fmap
  --       (\code -> toNumber code * (minimum . fmap length . getControlSeq 2) code)

getControlSeq2 :: Int -> String -> [Int]
getControlSeq2 n code =
  fmap (sum . M.elems)
    . (!! n)
    . iterate
        ((\sts ->
            let minS = minimum . fmap sum $ sts
             in filter ((== minS) . sum) sts)
           . S.toList
           . S.fromList
           . concatMap contSeq)
    . fmap (toState 1)
    . buildPathsFromParts
    $ digSeq
  where
    digSeq =
      fmap
        (\(c0, c1) ->
           let p0 = pointForChar c0
               p1 = pointForChar c1
            in fmap (<> [A]) . bfs digitalKeyboard p0 $ p1)
        . zip ('A' : code)
        $ code
    toState :: Int -> [Move] -> M.Map (Move, Move) Int
    toState k moves = M.fromListWith (+) . fmap (, k) . zip (A : moves) $ moves
    contSeq :: M.Map (Move, Move) Int -> [M.Map (Move, Move) Int]
    contSeq =
      foldl'
        (\sts ((m0, m1), k) ->
           [ M.unionWith (+) st . toState k $ moves
           | st <- sts
           , moves <- memoControlKeybMove m0 m1
           ])
        [M.empty]
        . M.toList

part2Solution :: [String] -> Int
part2Solution =
  sum . fmap (\code -> toNumber code * (minimum . getControlSeq2 25) code)
