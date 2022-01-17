module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Data.List (nub, sort)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data ObjName = Generator | Microchip deriving (Eq, Ord, Show)

instance Read ObjName where
  readPrec = ((lift . P.string) "generator" >> return Generator) PC.+++ ((lift . P.string) "microchip" >> return Microchip)

type ObjType = String

data Obj = Obj ObjName ObjType deriving (Eq, Ord, Show)

isGenerator :: Obj -> Bool
isGenerator (Obj Generator _) = True
isGenerator _ = False

isMicrochip :: Obj -> Bool
isMicrochip (Obj Microchip _) = True
isMicrochip _ = False

objType :: Obj -> String
objType (Obj _ t) = t

instance Read Obj where
  readPrec = do
    lift . P.string $ "a "
    t <- lift . P.munch $ isAlpha
    lift . P.many . P.string $ "-compatible"
    lift P.skipSpaces
    n <- readPrec
    return $ Obj n t

newtype Floor = Floor {unFloor :: S.Set Obj} deriving (Eq, Ord, Show)

instance Read Floor where
  readPrec = do
    lift . P.string $ "The "
    lift . P.munch $ isAlpha
    lift . P.string $ " floor contains "
    objs <- lift . P.many $ do
      P.many . P.char $ ','
      P.skipSpaces
      P.many . P.string $ "and "
      P.skipSpaces
      PC.readPrec_to_P readPrec 0
    lift . P.many . P.string $ "nothing relevant"
    lift . P.char $ '.'
    return . Floor . S.fromList $ objs

data Area = Area Int [Floor] deriving (Eq, Ord, Show)

mkArea :: [Floor] -> Area
mkArea = Area 0

pairs :: [a] -> [[a]]
pairs [] = []
pairs [a] = []
pairs (a : as) = map (: [a]) as ++ pairs as

checkFloor :: Floor -> Bool
checkFloor fl = S.null gens || S.null mchips || S.null (mchips S.\\ gens)
  where
    objs = unFloor fl
    mchips = S.map objType . S.filter isMicrochip $ objs
    gens = S.map objType . S.filter isGenerator $ objs

checkArea :: Area -> Bool
checkArea (Area _ fls) = all checkFloor fls

possibleMoves :: Area -> S.Set Area
possibleMoves (Area n fls) =
  S.fromList
    . filter checkArea
    . map updateArea
    $ [(n', S.fromList lobjs') | n' <- [n - 1, n + 1], n' >= 0, n' < length fls, lobjs' <- map (: []) lobjs ++ pairs lobjs]
  where
    lobjs = S.toList . unFloor $ fls !! n
    updateArea (n', objs') = Area n' (zipWith updateFloor [0 ..] fls)
      where
        updateFloor i fl@(Floor objs'')
          | i == n = Floor . S.filter (`S.notMember` objs') $ objs''
          | i == n' = Floor (objs' `S.union` objs'')
          | otherwise = fl

findShortestLength :: Area -> Int
findShortestLength a = doer S.empty (S.singleton a)
  where
    doer visited as
      | (any isFinal . S.toList) as = 0
      | otherwise = 1 + doer visited' ((S.unions . map possibleMoves . S.toList $ as) S.\\ visited')
      where
        visited' = as `S.union` visited
    isFinal (Area n fls) = n == 3 && (all (S.null . unFloor) . take 3) fls

part1Solution :: [Floor] -> Int
part1Solution = findShortestLength . mkArea

addToFloor :: Int -> S.Set Obj -> Area -> Area
addToFloor i objs (Area n fls) = Area n (zipWith (\i' fl -> if i' == i then (Floor . S.union objs . unFloor) fl else fl) [0 ..] fls)

part2Solution :: [Floor] -> Int
part2Solution =
  findShortestLength
    . addToFloor 0 (S.fromList [Obj Microchip "elerium", Obj Generator "elerium", Obj Microchip "dilithium", Obj Generator "dilithium"])
    . mkArea
