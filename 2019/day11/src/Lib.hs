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
import Data.Maybe (fromMaybe)
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

data Direction = U | D | L | R deriving (Show, Eq)

data Robot = Robot {rProgram :: Program, rPos :: Point, rDirection :: Direction, rPaint :: M.Map Point Int} deriving (Show)

mkRobot :: M.Map Point Int -> Program -> Robot
mkRobot paint pr = Robot (execute pr) (Point 0 0) U paint

turnRobot :: Int -> Robot -> Robot
turnRobot turn r
  | turn == 0 && d == U = r {rDirection = L}
  | turn == 0 && d == D = r {rDirection = R}
  | turn == 0 && d == L = r {rDirection = D}
  | turn == 0 && d == R = r {rDirection = U}
  | turn == 1 && d == U = r {rDirection = R}
  | turn == 1 && d == D = r {rDirection = L}
  | turn == 1 && d == L = r {rDirection = U}
  | turn == 1 && d == R = r {rDirection = D}
  | otherwise = error "wrong turn"
  where
    d = rDirection r

moveRobot :: Robot -> Robot
moveRobot r
  | d == U = r {rPos = pos {pY = pY pos - 1}}
  | d == D = r {rPos = pos {pY = pY pos + 1}}
  | d == R = r {rPos = pos {pX = pX pos + 1}}
  | d == L = r {rPos = pos {pX = pX pos - 1}}
  | otherwise = error "it's not possible"
  where
    d = rDirection r
    pos = rPos r

stepRobot :: Robot -> Robot
stepRobot r = moveRobot . turnRobot turn $ r {rProgram = p' {pOut = []}, rPaint = paint}
  where
    c = fromMaybe 0 $ M.lookup (rPos r) (rPaint r)
    p' = continue . setInput [c] . rProgram $ r
    [turn, c'] = pOut p'
    paint = M.insert (rPos r) c' . rPaint $ r

startRobot :: Robot -> Robot
startRobot r
  | status == Halted = r
  | status == Error = r
  | status == Wait = startRobot . stepRobot $ r
  | otherwise = r
  where
    status = (pStatus . rProgram) r

showPaint :: M.Map Point Int -> String
showPaint paint =
  intercalate
    "\n"
    [ [if fromMaybe 0 (M.lookup (Point x y) paint) == 0 then ' ' else '#' | x <- [xMin .. xMax]]
      | y <- [yMin .. yMax]
    ]
  where
    points = M.keysSet paint
    xs = S.map pX points
    ys = S.map pY points
    xMin = S.findMin xs
    xMax = S.findMax xs
    yMin = S.findMin ys
    yMax = S.findMax ys

part1Solution :: Program -> Int
part1Solution = M.size . rPaint . startRobot . mkRobot M.empty

part2Solution :: Program -> String
part2Solution = showPaint . rPaint . startRobot . mkRobot (M.singleton (Point 0 0) 1)
