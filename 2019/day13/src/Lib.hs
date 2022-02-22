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

data Arcade = Arcade {aPr :: Program, aBall :: Point, aPaddle :: Point} deriving (Show)

insertCoins :: Program -> Program
insertCoins pr = pr {pMem = insert 0 2 . pMem $ pr}

getTileP :: Int -> M.Map Point Int -> Maybe Point
getTileP t = fmap fst . M.lookupMin . M.filter (== t)

getPaddle :: M.Map Point Int -> Maybe Point
getPaddle = getTileP 3

getBall :: M.Map Point Int -> Maybe Point
getBall = getTileP 4

mkArcade :: Program -> Arcade
mkArcade pr = Arcade pr' (fromMaybe (Point 0 0) . getBall $ screen) (fromMaybe (Point 0 0) . getPaddle $ screen)
  where
    pr' = execute . insertCoins $ pr
    screen = parseOutput . getOut $ pr'

split :: Int -> [a] -> [[a]]
split n xs
  | length xs < n = []
  | otherwise = take n xs : split n (drop n xs)

parseOutput :: [Int] -> M.Map Point Int
parseOutput = M.fromList . map (\[n1, n2, n3] -> (Point n1 n2, n3)) . split 3

tileToChar :: Int -> Char
tileToChar 0 = ' '
tileToChar 1 = '#'
tileToChar 2 = '^'
tileToChar 3 = '='
tileToChar 4 = '*'
tileToChar _ = 'E'

showScreen :: M.Map Point Int -> String
showScreen screen =
  show score ++ "\n"
    ++ intercalate
      "\n"
      [ [tileToChar . fromMaybe 0 . M.lookup (Point x y) $ screen' | x <- [xMin .. xMax]]
        | y <- [yMin .. yMax]
      ]
  where
    p = Point (-1) 0
    score = fromMaybe 0 . M.lookup p $ screen
    screen' = M.filterWithKey (\p' _ -> p' /= p) screen
    points = M.keysSet screen'
    xs = S.map pX points
    ys = S.map pY points
    xMin = S.findMin xs
    xMax = S.findMax xs
    yMin = S.findMin ys
    yMax = S.findMax ys

decide :: Arcade -> (Int, Point, Point)
decide a
  | predictedLandingX > pX paddle = (1, ball, paddle)
  | predictedLandingX < pX paddle = (-1, ball, paddle)
  | otherwise = (0, ball, paddle)
  where
    screen = parseOutput . getOut . aPr $ a
    ball = fromMaybe (aBall a) . getBall $ screen
    direction = if pX ball < (pX . aBall) a then -1 else 1
    paddle = fromMaybe (aPaddle a) . getPaddle $ screen
    predictedLandingX = pX ball + direction * (pY paddle - pY ball - 1)

play :: Arcade -> Int
play a
  | status == Halted = fromMaybe 0 . M.lookup (Point (-1) 0) . parseOutput . getOut . aPr $ a
  | status == Wait = play $ a {aPr = continue $ (aPr a) {pOut = [], pIn = [inp]}, aPaddle = paddle, aBall = ball}
  | otherwise = error ("impossible state " ++ (show . pStatus . aPr) a)
  where
    status = pStatus . aPr $ a
    (inp, ball, paddle) = decide a

part1Solution :: Program -> Int
part1Solution pr = M.size . M.filter (== 2) . parseOutput . reverse . pOut $ pr'
  where
    pr' = execute pr

part2Solution :: Program -> Int
part2Solution = play . mkArcade
