{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Bifunctor (second)
import Data.IntMap (IntMap (..), findMax, findMin, fromList, insert, size)
import qualified Data.IntMap as IM
import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Status = Running | Halted | Wait | Error deriving (Show, Eq)

data Program = Program
  { pMem :: IntMap Int,
    pIn :: [Int],
    pOut :: [Int],
    pPointer :: Int,
    pStatus :: Status,
    pRelAddr :: Int,
    pId :: Int
  }
  deriving (Show)

instance Read Program where
  readPrec = do
    mem <- fmap (fromList . zip [0 ..]) . lift . P.many $ do
      n <- PC.readPrec_to_P readPrec 0
      P.optional . P.char $ ','
      return n
    return $ Program mem [] [] 0 Running 0 0

setInput :: [Int] -> Program -> Program
setInput ns p = p {pIn = ns}

setMem :: Int -> Int -> Program -> Program
setMem addr n p = p {pMem = insert addr n . pMem $ p}

getMem :: Int -> Program -> Int
getMem addr = (fromMaybe 0 . IM.lookup addr) . pMem

getOut :: Program -> [Int]
getOut = reverse . pOut

setRelAddr :: Int -> Program -> Program
setRelAddr addr p = p {pRelAddr = addr}

getModVal :: Int -> Int -> Program -> Int
getModVal m n p
  | m == 0 = getMem (getMem n p) p
  | m == 1 = getMem n p
  | m == 2 = getMem (pRelAddr p + getMem n p) p
  | otherwise = n

setModVal :: Int -> Int -> Int -> Program -> Program
setModVal m i val p
  | m == 0 = setMem (getMem i p) val p
  | m == 2 = setMem (pRelAddr p + getMem i p) val p
  | otherwise = p {pStatus = Error}

getFromInput :: Program -> (Int, Program)
getFromInput p = (n, p {pIn = ns})
  where
    (n : ns) = pIn p

output :: Int -> Program -> Program
output n p = p {pOut = n : pOut p}

goto :: Int -> Program -> Program
goto i p
  | i >= (fst . findMax . pMem) p || pPointer p < (fst . findMin . pMem) p = p {pStatus = Halted}
  | otherwise = p {pPointer = i}

nextOp :: Int -> Program -> Program
nextOp i p
  | pPointer p + i >= (fst . findMax . pMem) p || pPointer p < (fst . findMin . pMem) p = p {pStatus = Halted}
  | otherwise = p {pPointer = pPointer p + i}

binary :: (Int -> Int -> Int) -> Int -> Int -> Int -> Int -> Program -> Program
binary op i m1 m2 m3 p = setModVal m3 (i + 3) (v1 `op` v2) p
  where
    v1 = getModVal m1 (i + 1) p
    v2 = getModVal m2 (i + 2) p

applyOp :: Program -> Program
applyOp p
  | op == 1 = nextOp 4 . binary (+) i m1 m2 m3 $ p
  | op == 2 = nextOp 4 . binary (*) i m1 m2 m3 $ p
  | op == 3 && (null . pIn) p = p {pStatus = Wait}
  | op == 3 = nextOp 2 . setModVal m1 (i + 1) inp $ p'
  | op == 4 = nextOp 2 . output v1 $ p
  | op == 5 = if v1 /= 0 then goto v2 p else nextOp 3 p
  | op == 6 = if v1 == 0 then goto v2 p else nextOp 3 p
  | op == 7 = nextOp 4 . setModVal m3 (i + 3) (if v1 < v2 then 1 else 0) $ p
  | op == 8 = nextOp 4 . setModVal m3 (i + 3) (if v1 == v2 then 1 else 0) $ p
  | op == 9 = nextOp 2 . setRelAddr (pRelAddr p + v1) $ p
  | op == 99 = p {pStatus = Halted}
  | otherwise = p {pStatus = Error}
  where
    i = pPointer p
    n = getMem (pPointer p) p
    op = n `mod` 100
    m1 = n `div` 100 `mod` 10
    m2 = n `div` 1000 `mod` 10
    m3 = n `div` 10000 `mod` 10
    v1 = getModVal m1 (i + 1) p
    v2 = getModVal m2 (i + 2) p
    (inp, p') = getFromInput p

execute :: Program -> Program
execute p
  | pStatus p /= Running = p
  | otherwise = execute . applyOp $ p

continue :: Program -> Program
continue p
  | pStatus p == Running || pStatus p == Wait = execute $ p {pStatus = Running}
  | otherwise = p

chainExec :: Program -> Int -> [Int] -> Int
chainExec p = foldl' (\out n -> head . pOut . execute . setInput [n, out] $ p)

chainProgramExec :: Int -> [Program] -> [Program]
chainProgramExec n ps
  | (pStatus . last) ps /= Wait = ps
  | otherwise = chainProgramExec n' ps'
  where
    (n', ps') = second reverse . foldl' doer (n, []) $ ps
    doer :: (Int, [Program]) -> Program -> (Int, [Program])
    doer (n'', ps'') p = ((head . pOut) p', p' : ps'')
      where
        p' = continue $ p {pIn = [n''], pOut = []}

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

data Droid = Droid {dPr :: Program, dOS :: Point, dMap :: M.Map Point Int, dDr :: Point} deriving (Show)

mkDroid :: Program -> Droid
mkDroid pr = Droid {dPr = pr, dOS = Point 0 0, dMap = M.singleton (Point 0 0) 1, dDr = Point 0 0}

tileToChar :: Int -> Char
tileToChar 0 = '#'
tileToChar 1 = '.'
tileToChar 5 = ' '
tileToChar _ = 'E'

showArea :: Droid -> String
showArea d =
  intercalate
    "\n"
    [[showTile (Point x y) | x <- [xMin .. xMax]] | y <- [yMin .. yMax]]
  where
    p = Point (-1) 0
    area = dMap d
    points = M.keysSet area
    xs = S.map pX points
    ys = S.map pY points
    xMin = S.findMin xs
    xMax = S.findMax xs
    yMin = S.findMin ys
    yMax = S.findMax ys
    showTile p
      | p == dOS d = 'O'
      | p == dDr d = '*'
      | p == Point 0 0 = '+'
      | otherwise = tileToChar . fromMaybe 5 . M.lookup p $ area

pointsAround :: Point -> [(Int, Point)]
pointsAround p = [(3, p {pX = pX p + 1}), (4, p {pX = pX p - 1}), (1, p {pY = pY p + 1}), (2, p {pY = pY p - 1})]

buildDistanceMap :: Point -> Maybe Point -> M.Map Point Int -> (M.Map Point Int, (Maybe Point, Int))
buildDistanceMap pFrom mbpTo area = doer M.empty [pFrom] 0
  where
    doer :: M.Map Point Int -> [Point] -> Int -> (M.Map Point Int, (Maybe Point, Int))
    doer area' front step
      | null front = (area', (Nothing, step - 1))
      | isJust mbpTo && pTo `elem` front = (area'', (Just pTo, step))
      | (not . null) unknowns = (\(p, _) -> (area'', (Just p, step))) . head $ unknowns
      | otherwise = doer area'' front' (step + 1)
      where
        pTo = fromMaybe (Point 0 0) mbpTo
        area'' = M.union area' . M.fromList . map (,step) $ front
        ps = map (\p -> (p, p `M.lookup` area)) front
        unknowns = filter (isNothing . snd) ps
        front' =
          filter (\p -> p `M.notMember` area'' && p `M.lookup` area /= Just 0)
            . map snd
            . concatMap pointsAround
            $ front

findPath :: Point -> Int -> M.Map Point Int -> [(Int, Point)]
findPath p step area
  | step == 1 = [dp]
  | otherwise = dp : findPath (snd dp) (step - 1) area
  where
    dp = head . filter (\(_, p') -> p' `M.lookup` area == Just (step - 1)) . pointsAround $ p

findPathToUnknown :: Droid -> Maybe ([Int], Point, Point)
findPathToUnknown d = case buildDistanceMap (dDr d) Nothing (dMap d) of
  (area, (Just p, maxStep)) -> Just (path, p, pFrom)
    where
      ps = reverse $ findPath p maxStep area
      pFrom = snd . last $ ps
      path = map fst ps
  _ -> Nothing

explore :: Droid -> Droid
explore d = case findPathToUnknown d of
  Just (path, p, pFrom) -> explore $ d {dPr = pr', dDr = p', dOS = if tile == 2 then p else dOS d, dMap = area}
    where
      pr = continue . setInput path . dPr $ d
      pr' = pr {pOut = [], pIn = path}
      tile = head . pOut $ pr
      p' = if tile == 0 then pFrom else p
      tile' = if tile == 2 then 1 else tile
      area = M.insert p tile' . dMap $ d
  _ -> d

minDist :: Droid -> Int
minDist d = snd . snd . buildDistanceMap (Point 0 0) ((Just . dOS) d) . dMap $ d

fillOxygen :: Droid -> Int
fillOxygen d = snd . snd . buildDistanceMap (dOS d) Nothing . dMap $ d

part1Solution :: Program -> Int
part1Solution = minDist . explore . mkDroid

part2Solution :: Program -> Int
part2Solution = fillOxygen . explore . mkDroid
