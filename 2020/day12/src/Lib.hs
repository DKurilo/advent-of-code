-- Interesting thing
-- Firstly I had Double everywhere, because there is no limitation on degrees amount in the puzzle descripition
-- So I had Double everywhere. But then I found for the second part rouding error is so huge that it's affects
-- result very significantly.
-- So first I add round on each step, but I still had rounding error.
-- So I was need to implemet 90-degrees related rotation as coordinates changing.
-- There is no directions other then North, South, East, West. So it works.
-- But I still don't like this task description.
module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import           Data.Bifunctor               (second)
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

newtype Units = U Int deriving (Show, Eq)

instance Read Units where
    readPrec = U <$> readPrec

newtype Degrees = D Int deriving (Show, Eq)

instance Read Degrees where
    readPrec = D <$> readPrec

data Instruction = N Units | S Units | E Units | W Units | L Degrees | R Degrees | F Units

instance Read Instruction where
    readPrec =
        (do
            lift $ P.char 'N'
            N <$> readPrec)
        <|>
        (do
            lift $ P.char 'S'
            S <$> readPrec)
        <|>
        (do
            lift $ P.char 'E'
            E <$> readPrec)
        <|>
        (do
            lift $ P.char 'W'
            W <$> readPrec)
        <|>
        (do
            lift $ P.char 'L'
            L <$> readPrec)
        <|>
        (do
            lift $ P.char 'R'
            R <$> readPrec)
        <|>
        (do
            lift $ P.char 'F'
            F <$> readPrec)

type Angle = Int -- East is 0
type SNShift = Int -- S is > 0
type EWShift = Int -- E is > 0

data Waypoint = WP SNShift EWShift deriving (Show)

data Coords = Coords Angle SNShift EWShift Waypoint deriving (Show)

manhattan :: Coords -> Int
manhattan (Coords _ sns ews _) = abs sns + abs ews

initCoords :: Coords
initCoords = Coords 0 0 0 (WP (-1) 10)

navigate1 :: Coords -> [Instruction] -> Coords
navigate1 = foldl go
    where go :: Coords -> Instruction -> Coords
          go (Coords a sns ews w) (N (U x)) = Coords a (sns - x) ews w
          go (Coords a sns ews w) (S (U x)) = Coords a (sns + x) ews w
          go (Coords a sns ews w) (E (U x)) = Coords a sns (ews + x) w
          go (Coords a sns ews w) (W (U x)) = Coords a sns (ews - x) w
          go (Coords a sns ews w) (L (D x)) = Coords (a - x) sns ews w
          go (Coords a sns ews w) (R (D x)) = Coords (a + x) sns ews w
          go (Coords a sns ews w) (F (U x)) = Coords a (sns + round (fromIntegral x * sin ar)) (ews + round(fromIntegral x * cos ar)) w
              where ar = fromIntegral a * pi / 180

toPolar :: (Int, Int) -> (Double, Int)
toPolar (sn, ew) = (sqrt (sn' * sn' + ew' * ew'), round (atan (sn' / ew') * 180 / pi))
    where sn' = fromIntegral sn
          ew' = fromIntegral ew

fromPolar :: (Double, Int) -> (Int, Int)
fromPolar (ρ, α) = (round (ρ * sin αr),round ( ρ * cos αr))
    where αr = fromIntegral α * pi / 180

rotate :: Angle ->  Waypoint -> Waypoint
rotate a wp@(WP sn ew)
    | a `mod` 90 == 0 && a >= 0 = foldl (\(WP sn' ew') _ -> WP ew' (-sn')) wp [90, 180..a]
    | a `mod` 90 == 0 && a < 0 = foldl (\(WP sn' ew') _ -> WP (-ew') sn') wp [(-90), (-180)..a]
    | otherwise = WP sn' ew'
    where (sn', ew') = (fromPolar . second (+a) . toPolar) (sn, ew)

navigate2 :: Coords -> [Instruction] -> Coords
navigate2 = foldl go
    where go :: Coords -> Instruction -> Coords
          go (Coords a sns ews (WP wsns wews)) (N (U x)) = Coords a sns ews (WP (wsns - x) wews)
          go (Coords a sns ews (WP wsns wews)) (S (U x)) = Coords a sns ews (WP (wsns + x) wews)
          go (Coords a sns ews (WP wsns wews)) (E (U x)) = Coords a sns ews (WP wsns (wews + x))
          go (Coords a sns ews (WP wsns wews)) (W (U x)) = Coords a sns ews (WP wsns (wews - x))
          go (Coords a sns ews wp) (L (D x)) = Coords a sns ews (rotate (-x) wp)
          go (Coords a sns ews wp) (R (D x)) = Coords a sns ews (rotate x wp)
          go (Coords a sns ews wp@(WP wsns wews)) (F (U x)) = Coords a (sns + x * wsns) (ews + x * wews) wp

input :: IO [Instruction]
input = map read . filter (not . null) . lines <$> readFile "input"

part1solution :: IO ()
part1solution = print . manhattan . navigate1 initCoords =<< input

part2solution :: IO ()
part2solution = print .manhattan . navigate2 initCoords =<< input
