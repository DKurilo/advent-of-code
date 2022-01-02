module Lib
    ( part1solution
    , part2solution
    ) where

import           Data.Function (fix)
import qualified Data.Vector   as V
import           Debug.Trace

data Seat = Empty | Occupied | Floor deriving (Eq, Show)

type Floor = V.Vector (V.Vector Seat)

showFloor :: Floor -> String
showFloor = V.foldr (\row cs -> V.foldr (\s cs' -> (case s of
                                                      Empty    -> 'L'
                                                      Occupied -> '#'
                                                      Floor    -> '.') : cs') "" row ++ "\n" ++ cs) ""

charToSeat :: Char -> Seat
charToSeat c = case c of
                   'L' -> Empty
                   '#' -> Occupied
                   '.' -> Floor

input :: IO Floor
input = V.fromList . map (V.fromList . map charToSeat) . filter (not. null) . lines <$> readFile "input"

count :: Seat -> Floor -> Int
count s = V.sum . V.map (V.sum . V.map (\s' -> if s' == s then 1 else 0))

countNearestAdjascent :: Floor -> (Seat -> Bool) -> Int -> Int -> Int
countNearestAdjascent f p i j = (if hasT && hasL && p (f V.! (i - 1) V.! (j - 1)) then 1 else 0)
                              + (if hasT && p (f V.! (i - 1) V.! j) then 1 else 0)
                              + (if hasT && hasR && p (f V.! (i - 1) V.! (j + 1)) then 1 else 0)
                              + (if hasL && p (f V.! i V.! (j - 1)) then 1 else 0)
                              + (if hasR && p (f V.! i V.! (j + 1)) then 1 else 0)
                              + (if hasB && hasL && p (f V.! (i + 1) V.! (j - 1)) then 1 else 0)
                              + (if hasB && p (f V.! (i + 1) V.! j) then 1 else 0)
                              + (if hasB && hasR && p (f V.! (i + 1) V.! (j + 1)) then 1 else 0)
    where hasT = i > 0
          hasB = i < V.length f - 1
          hasL = j > 0
          hasR = j < V.length (f V.! 0) - 1

countSightAdjascent :: Floor -> (Seat -> Bool) -> Int -> Int -> Int
countSightAdjascent f p i j = canSee dec dec f p i j
                            + canSee dec id f p i j
                            + canSee dec inc f p i j
                            + canSee id dec f p i j
                            + canSee id inc f p i j
                            + canSee inc dec f p i j
                            + canSee inc id f p i j
                            + canSee inc inc f p i j
    where canSee :: (Int -> Int) -> (Int -> Int) -> Floor -> (Seat -> Bool) -> Int -> Int -> Int
          canSee di dj f p i j
            | isOnFloor && p (f V.! i' V.! j') = 1
            | isOnFloor && f V.! i' V.! j' == Floor = canSee di dj f p i' j'
            | otherwise = 0
              where i' = di i
                    j' = dj j
                    isOnFloor = i' >= 0 && i' < V.length f && j' >= 0 && j' < V.length (f V.! 0)
          dec x = x - 1
          inc = (+1)

step :: (Floor -> (Seat -> Bool) -> Int -> Int -> Int) -> Int -> (Floor -> Floor -> Floor) -> Floor -> Floor -> Floor
step countAdjascent toleranceAround rec f f'
  | f' == f = f
  | otherwise = rec f' next
    where next :: Floor
          next = V.imap (\i -> V.imap (\j s -> transformSeat s i j)) f'
          transformSeat :: Seat -> Int -> Int -> Seat
          transformSeat Empty i j | countAdjascent f' (==Occupied) i j == 0 = Occupied
          transformSeat Occupied i j | countAdjascent f' (==Occupied) i j >= toleranceAround = Empty
          transformSeat s _ _ = s

part1solution :: IO ()
part1solution = print . count Occupied . fix (step countNearestAdjascent 4) V.empty  =<< input

part2solution :: IO ()
part2solution = print . count Occupied . fix (step countSightAdjascent 5) V.empty  =<< input
