module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Monad (guard)
import Data.Bifunctor (second)
import Data.Char (chr, ord)
import Data.IntMap (IntMap (..), findMax, findMin, fromList, insert, size)
import qualified Data.IntMap as IM
import Data.List (foldl', intercalate, isInfixOf, isPrefixOf)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, isJust, isNothing, maybe)
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

isPulled :: Int -> Int -> Program -> Bool
isPulled x y pr = (head . pOut . execute) (pr {pIn = [x, y]}) == 1

part1Solution :: Program -> Int
part1Solution pr = length . filter id $ [isPulled x y pr | x <- [0 .. 49], y <- [0 .. 49]]

shipSize = 100

binarySearch :: (Int -> Bool) -> Int -> Int -> Int
binarySearch p start end
  | end == start = end
  | end - start == 1 = if p end then end else start
  | p middle = binarySearch p middle end
  | otherwise = binarySearch p start middle
  where
    middle = (start + end) `div` 2

rightEdge :: Int -> Int -> Program -> Int
rightEdge y k100 pr = binarySearch (\n -> isPulled n y pr) ((y * k100) `div` 100) (y * 5)

isGood :: Int -> Int -> Program -> Bool
isGood y k100 pr = isPulled x y pr && isPulled (x - shipSize + 1) (y + shipSize - 1) pr
  where
    x = rightEdge y k100 pr

getK100 :: Program -> Int
getK100 pr = (xStart + xEnd) `div` 2
  where
    xStart = last . takeWhile (\x -> not $ isPulled x 100 pr) $ [0 ..]
    xEnd = head . takeWhile (\x -> isPulled x 100 pr) $ [xStart + 1 ..]

showBeam :: Program -> Int -> Int -> Int -> Int -> Int -> Int -> String
showBeam pr xFrom xTo yFrom yTo xLeft yTop =
  intercalate
    "\n"
    [ [ if isPulled x y pr
          then if x >= xLeft && x < xLeft + shipSize && y >= yTop && y < yTop + shipSize then 'O' else '#'
          else '.'
        | x <- [xFrom .. xTo]
      ]
      | y <- [yFrom .. yTo]
    ]

part2Solution :: Program -> Int
part2Solution pr = yTop + xLeft * 10000
  where
    k100 = getK100 pr
    yTopStart = binarySearch (\y -> not $ isGood y k100 pr) 0 10000000 + 1
    yTop = head . filter (\y -> isGood y k100 pr) $ [yTopStart - 100 .. yTopStart]
    xRight = rightEdge yTop k100 pr
    xLeft = xRight - shipSize + 1
