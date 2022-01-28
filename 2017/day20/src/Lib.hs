module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (nub, sort)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Particle = Particle
  { n :: Int,
    x :: Int,
    y :: Int,
    z :: Int,
    vx :: Int,
    vy :: Int,
    vz :: Int,
    ax :: Int,
    ay :: Int,
    az :: Int
  }
  deriving (Eq, Ord, Show)

instance Read Particle where
  readPrec = do
    lift . P.string $ "p=<"
    x <- readPrec
    lift . P.char $ ','
    y <- readPrec
    lift . P.char $ ','
    z <- readPrec
    lift . P.string $ ">, v=<"
    vx <- readPrec
    lift . P.char $ ','
    vy <- readPrec
    lift . P.char $ ','
    vz <- readPrec
    lift . P.string $ ">, a=<"
    ax <- readPrec
    lift . P.char $ ','
    ay <- readPrec
    lift . P.char $ ','
    az <- readPrec
    lift . P.char $ '>'
    return $ Particle 0 x y z vx vy vz ax ay az

part1Solution :: [Particle] -> Int
part1Solution =
  snd . minimum
    . zipWith
      ( \i p ->
          ( ( abs (ax p) + abs (ay p) + abs (az p),
              abs (vx p) + abs (vy p) + abs (vz p),
              abs (x p) + abs (y p) + abs (z p)
            ),
            i
          )
      )
      [0 ..]

solveEq :: Int -> Int -> Int -> Int -> Int -> Int -> Maybe [Int]
solveEq ai aj vi vj i j
  | a == 0 && b == 0 && c == 0 = Nothing
  | a == 0 && b == 0 = Just []
  | a == 0 = Just [t | t <- [(- (round c)) `div` round b], t > 0, checkTime t]
  | d < 0 = Just []
  | otherwise = Just [t | t <- [t1, t2], t > 0 && checkTime t]
  where
    a = fromIntegral $ ai - aj
    b = fromIntegral $ 2 * (vi - vj) + ai - aj
    c = fromIntegral $ 2 * (i - j)
    d = b * b - 4 * a * c
    t1 = round $ (- b - sqrt d) / (2 * a)
    t2 = round $ (- b + sqrt d) / (2 * a)
    checkTime t = (i + vi * t + ai * t * (t + 1) `div` 2) == (j + vj * t + aj * t * (t + 1) `div` 2)

collisionTimes :: Particle -> Particle -> [(Int, Particle, Particle)]
collisionTimes pi pj = case (mtxs, mtys, mtzs) of
  (Nothing, Nothing, Nothing) -> [(0, pi, pj)]
  (Nothing, Nothing, Just tzs) -> [(t, pi, pj) | t <- tzs]
  (Nothing, Just tys, Nothing) -> [(t, pi, pj) | t <- tys]
  (Nothing, Just tys, Just tzs) -> [(t, pi, pj) | t <- tys, t' <- tzs, t == t']
  (Just txs, Nothing, Nothing) -> [(t, pi, pj) | t <- txs]
  (Just txs, Nothing, Just tzs) -> [(t, pi, pj) | t <- txs, t' <- tzs, t == t']
  (Just txs, Just tys, Nothing) -> [(t, pi, pj) | t <- txs, t' <- tys, t == t']
  (Just txs, Just tys, Just tzs) -> [(tx, pi, pj) | tx <- txs, ty <- tys, tz <- tzs, tx == ty && ty == tz]
  where
    mtxs = solveEq (ax pi) (ax pj) (vx pi) (vx pj) (x pi) (x pj)
    mtys = solveEq (ay pi) (ay pj) (vy pi) (vy pj) (y pi) (y pj)
    mtzs = solveEq (az pi) (az pj) (vz pi) (vz pj) (z pi) (z pj)

removeCollisions :: [Particle] -> [Particle]
removeCollisions ps = doer ps collisions
  where
    collisions = sort . concat $ [collisionTimes pi pj | pi <- ps, pj <- ps, n pi < n pj]
    doer ps' [] = ps'
    doer ps' cs@((t, _, _) : _) = doer ps'' cs'
      where
        collided = nub . concatMap (\(_, pi, pj) -> [pi, pj]) . filter (\(t', _, _) -> t == t') $ cs
        ps'' = filter (`notElem` collided) ps'
        cs' = filter (\(_, pi, pj) -> pi `notElem` collided && pj `notElem` collided) cs

part2Solution :: [Particle] -> Int
part2Solution ps = length . removeCollisions $ zipWith (\i p -> p {n = i}) [0 ..] ps
