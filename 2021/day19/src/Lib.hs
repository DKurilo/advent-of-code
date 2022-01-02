module Lib
  ( solution,
    Scanner (..),
    readMany,
  )
where

import Control.Monad (guard)
import Data.List (intersect, nub)
import Data.Maybe (listToMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

-- from task we have at least 12 probes intersected
commonProbes = 12

-- https://hackage.haskell.org/package/utility-ht-0.0.16/docs/src/Text.Read.HT.html#readMany
readMany :: (Read a) => String -> [a]
readMany x =
  let contReadList [] = []
      contReadList [y] = fst y : readMany (snd y)
      contReadList _ = error "readMany: ambiguous parses"
   in contReadList (reads x)

data Axe = X | Y | Z deriving (Eq, Show)

data Dir = Pos | Neg deriving (Eq, Show)

type X = Int

type Y = Int

type Z = Int

data Point = P X Y Z deriving (Eq, Show)

instance Read Point where
  readPrec = do
    x <- readPrec
    (lift . P.char) ','
    y <- readPrec
    (lift . P.char) ','
    P x y <$> readPrec

data Scanner = Scanner Int Point [Point] deriving (Show)

instance Read Scanner where
  readPrec = do
    (lift . P.string) "--- scanner "
    sid <- readPrec
    (lift . P.string) " ---\n"
    Scanner sid (P 0 0 0) <$> (lift . P.many) (PC.readPrec_to_P readPrec 0)

newtype Space = Space [Scanner] deriving (Show)

fromScanner sc = Space [sc]

probes (Space scs) = nub . concatMap (\(Scanner _ _ ps) -> ps) $ scs

addToSpace :: Scanner -> Space -> Maybe Space
addToSpace sc (Space scs) = Space . (: scs) <$> listToMaybe scs'
  where
    scs' =
      foldl
        ( \scs'' sc' -> case toCommon sc sc' of
            Just sc'' -> sc'' : scs''
            _ -> scs''
        )
        []
        scs

toCommon :: Scanner -> Scanner -> Maybe Scanner
toCommon (Scanner sid scp ps) (Scanner _ _ fps) =
  listToMaybe
    ( do
        fp <- fps
        p <- ps
        mapX <- [X, Y, Z]
        mapY <- [X, Y, Z]
        mapZ <- [X, Y, Z]
        dirX <- [Pos, Neg]
        dirY <- [Pos, Neg]
        dirZ <- [Pos, Neg]
        -- we are not collapsing dimensions
        guard $ mapX /= mapY && mapX /= mapZ && mapY /= mapZ
        -- and scanner can't see probe's reflections
        guard . not $ isMirror mapX dirX mapY dirY mapZ dirZ
        let adjuster = adjust mapX dirX mapY dirY mapZ dirZ fp p
            adjustedPs = map adjuster ps
            commonPs = fps `intersect` adjustedPs
        guard $ length commonPs >= commonProbes
        return $ Scanner sid (adjuster scp) adjustedPs
    )

adjust :: Axe -> Dir -> Axe -> Dir -> Axe -> Dir -> Point -> Point -> Point -> Point
adjust mapX dirX mapY dirY mapZ dirZ (P fx fy fz) p p' = P x y z
  where
    get :: Axe -> Dir -> Point -> Int
    get X Pos (P x' _ _) = x'
    get X Neg (P x' _ _) = - x'
    get Y Pos (P _ y' _) = y'
    get Y Neg (P _ y' _) = - y'
    get Z Pos (P _ _ z') = z'
    get Z Neg (P _ _ z') = - z'
    x = fx - get mapX dirX p + get mapX dirX p'
    y = fy - get mapY dirY p + get mapY dirY p'
    z = fz - get mapZ dirZ p + get mapZ dirZ p'

buildSpace :: [Scanner] -> Space
buildSpace [] = Space []
buildSpace (sc : scs) = doer (fromScanner sc) scs
  where
    doer :: Space -> [Scanner] -> Space
    doer sp [] = sp
    doer sp (sc' : scs') = case addToSpace sc' sp of
      Just sp' -> doer sp' scs'
      _ -> doer sp (scs' ++ [sc'])

distance :: Point -> Point -> Int
distance (P x1 y1 z1) (P x2 y2 z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

isMirror :: Axe -> Dir -> Axe -> Dir -> Axe -> Dir -> Bool
isMirror mapX dirX mapY dirY mapZ dirZ = xy /= applyDir dirZ 1 || yz /= applyDir dirX 1 || zx /= applyDir dirY 1
  where
    applyDir :: Dir -> Int -> Int
    applyDir Pos n = n
    applyDir Neg n = - n
    getVec axe dir = case axe of
      X -> (applyDir dir 1, 0, 0)
      Y -> (0, applyDir dir 1, 0)
      Z -> (0, 0, applyDir dir 1)
    vx = getVec mapX dirX
    vy = getVec mapY dirY
    vz = getVec mapZ dirZ
    crossProdSum (x1, y1, z1) (x2, y2, z2) = y1 * z2 - z1 * y2 + z1 * x2 - x1 * z2 + x1 * y2 - y1 * x2
    xy = crossProdSum vx vy
    yz = crossProdSum vy vz
    zx = crossProdSum vz vx

solution :: [Scanner] -> (Int, Int)
solution scs = (length . probes $ sp, maximum [distance p1 p2 | (Scanner _ p1 _) <- scs', (Scanner _ p2 _) <- scs'])
  where
    sp@(Space scs') = buildSpace scs
