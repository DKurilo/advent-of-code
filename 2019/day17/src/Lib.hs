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
import Data.List.Split (splitOn)
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

data Point = Point {pX :: Int, pY :: Int} deriving (Eq, Ord, Show)

data Direction = U | D | L | R | X deriving (Show, Eq)

data Scaffold = Scaffold {scPr :: Program, scMap :: S.Set Point, scRp :: Point, scRd :: Direction} deriving (Show)

parseMap :: [Int] -> (S.Set Point, [(Point, Direction)])
parseMap =
  foldl'
    ( \res (y, cs) ->
        foldl'
          ( \(m, rs) (x, c) -> case c of
              '.' -> (m, rs)
              '#' -> (S.insert (Point x y) m, rs)
              '^' -> (S.insert (Point x y) m, (Point x y, U) : rs)
              'v' -> (S.insert (Point x y) m, (Point x y, D) : rs)
              '>' -> (S.insert (Point x y) m, (Point x y, R) : rs)
              '<' -> (S.insert (Point x y) m, (Point x y, L) : rs)
              _ -> (m, rs)
          )
          res
          . zip [0 ..]
          $ cs
    )
    (S.empty, [])
    . zip [0 ..]
    . lines
    . map chr

mkScaffold :: Program -> Scaffold
mkScaffold pr = Scaffold pr'' m p d
  where
    pr' = execute pr
    pr'' = pr' {pOut = []}
    (m, [(p, d)]) = parseMap . getOut $ pr'

showScaffold :: Scaffold -> String
showScaffold scf =
  intercalate
    "\n"
    [[showTile (Point x y) | x <- [xMin .. xMax]] | y <- [yMin .. yMax]]
  where
    p = Point (-1) 0
    area = scMap scf
    xs = S.map pX area
    ys = S.map pY area
    xMin = S.findMin xs
    xMax = S.findMax xs
    yMin = S.findMin ys
    yMax = S.findMax ys
    showTile p
      | p == scRp scf = case scRd scf of
        U -> '^'
        D -> 'v'
        R -> '>'
        L -> '<'
        X -> 'X'
      | p `S.member` area = '#'
      | otherwise = ' '

pointsAround :: Point -> [Point]
pointsAround p = [p {pX = pX p + 1}, p {pX = pX p - 1}, p {pY = pY p + 1}, p {pY = pY p - 1}]

data Action = AL | AR | AF Int deriving (Eq)

instance Show Action where
  show AL = "L"
  show AR = "R"
  show (AF n) = show n

findAllPaths :: Scaffold -> [[Action]]
findAllPaths scf = doer ((S.singleton . scRp) scf) (scRp scf) (scRd scf)
  where
    doer :: S.Set Point -> Point -> Direction -> [[Action]]
    doer visited rp rd
      | lengthWaysAround == 1 && rp /= scRp scf && S.size visited == (S.size . scMap) scf = [[]]
      | lengthWaysAround == 1 && S.size visited > 1 = []
      | null allWays = []
      | otherwise = allWays
      where
        lengthWaysAround = length . filter (`S.member` scMap scf) . pointsAround $ rp
        pFwd
          | rd == U = rp {pY = pY rp - 1}
          | rd == D = rp {pY = pY rp + 1}
          | rd == L = rp {pX = pX rp - 1}
          | rd == R = rp {pX = pX rp + 1}
          | otherwise = rp
        goForward
          | pFwd `S.member` scMap scf && (lengthWaysAround == 4 && pFwd `S.notMember` visited || lengthWaysAround < 4) =
            map (AF 1 :) $ doer (S.insert pFwd visited) pFwd rd
          | otherwise = []
        pLeft
          | rd == U = rp {pX = pX rp - 1}
          | rd == D = rp {pX = pX rp + 1}
          | rd == L = rp {pY = pY rp + 1}
          | rd == R = rp {pY = pY rp - 1}
          | otherwise = rp
        dLeft
          | rd == U = L
          | rd == D = R
          | rd == L = D
          | rd == R = U
          | otherwise = rd
        goLeft
          | pLeft `S.member` scMap scf && (lengthWaysAround == 2 || pLeft `S.notMember` visited) =
            map (\as -> AL : AF 1 : as) $ doer (S.insert pLeft visited) pLeft dLeft
          | otherwise = []
        pRight
          | rd == U = rp {pX = pX rp + 1}
          | rd == D = rp {pX = pX rp - 1}
          | rd == L = rp {pY = pY rp - 1}
          | rd == R = rp {pY = pY rp + 1}
          | otherwise = rp
        dRight
          | rd == U = R
          | rd == D = L
          | rd == L = U
          | rd == R = D
          | otherwise = rd
        goRight
          | pRight `S.member` scMap scf && (lengthWaysAround == 2 || pRight `S.notMember` visited) =
            map (\as -> AR : AF 1 : as) $ doer (S.insert pRight visited) pRight dRight
          | otherwise = []
        allWays = goForward ++ goRight ++ goLeft

data Function = A | B | C

instance Show Function where
  show A = "A"
  show B = "B"
  show C = "C"

data CPath = CPath {cpA :: [Action], cpB :: [Action], cpC :: [Action], cpP :: [Function], cpDebug :: Bool}

showFunc :: [Action] -> String
showFunc = intercalate "," . map show

showMain :: [Function] -> String
showMain = intercalate "," . map show

instance Show CPath where
  show cp =
    (showMain . cpP) cp
      <> "\n"
      <> (showFunc . cpA) cp
      <> "\n"
      <> (showFunc . cpB) cp
      <> "\n"
      <> (showFunc . cpC) cp
      <> "\n"
      <> (if cpDebug cp then "y" else "n")
      <> "\n"

shrinkPath :: [Action] -> [Action]
shrinkPath [] = []
shrinkPath (AR : as) = AR : shrinkPath as
shrinkPath (AL : as) = AL : shrinkPath as
shrinkPath (AF n : AF 1 : as) = shrinkPath $ AF (n + 1) : as
shrinkPath (AF n : as) = AF n : shrinkPath as

mkMain :: [Action] -> [Action] -> [Action] -> [Action] -> [Function]
mkMain pathA pathB pathC as
  | null as = []
  | pathA `isPrefixOf` as = A : (mkMain pathA pathB pathC . drop (length pathA)) as
  | pathB `isPrefixOf` as = B : (mkMain pathA pathB pathC . drop (length pathB)) as
  | pathC `isPrefixOf` as = C : (mkMain pathA pathB pathC . drop (length pathC)) as
  | otherwise = []

shrinkedLength :: [Action] -> Int
shrinkedLength = length . showFunc

possiblePart :: [[Action]] -> [([Action], [Action])]
possiblePart actss = takeWhile ((<= 20) . shrinkedLength . snd) $ do
  l <- [1 .. (length . head) actss]
  let path = take l . head $ actss
      shrinkedPath = shrinkPath path
  return (path, shrinkedPath)

convolutePath :: [Action] -> Maybe CPath
convolutePath actions
  | null cpaths = Nothing
  | otherwise = Just . head $ cpaths
  where
    cpaths = do
      (pathA, shrinkedPathA) <- possiblePart [actions]
      let restA = filter (not . null) . splitOn pathA $ actions
      (pathB, shrinkedPathB) <- possiblePart restA
      let restB = filter (not . null) . concatMap (splitOn pathB) $ restA
      (pathC, shrinkedPathC) <- possiblePart restB
      let restC = filter (not . null) . concatMap (splitOn pathC) $ restB
      guard $ null restC
      let mn = mkMain pathA pathB pathC actions
      guard $ length mn <= 20
      return $ CPath shrinkedPathA shrinkedPathB shrinkedPathC mn False

convolutePath' :: [Action] -> Maybe CPath
convolutePath' actions
  | null restAsC && (length . showMain) pr <= 20 = Just $ CPath shrinkedPathA shrinkedPathB shrinkedPathC (mkPr actions) False
  | otherwise = Nothing
  where
    getPart :: [Action] -> [[Action]] -> [Action]
    getPart [] ((a : firstActs) : actss) = getPart [a] (firstActs : actss)
    getPart as [] = as
    getPart as ([] : restActss) = getPart as restActss
    getPart as actss@((a : firstActs) : restActss)
      | shrinkedAsLength > 20 = as
      | rest nextAs < rest as = nextAs
      | otherwise = as
      where
        as' = as ++ [a]
        shrinkedAsLength = (length . showFunc . shrinkPath) as'
        rest as'' = length . concat . concatMap (splitOn as'') $ ((as ++ [a] ++ firstActs) : restActss)
        nextAs = getPart as' (firstActs : restActss)
    pathA = getPart [] [actions]
    shrinkedPathA = shrinkPath pathA
    restAsA = filter (not . null) . splitOn pathA $ actions
    pathB = getPart [] restAsA
    shrinkedPathB = shrinkPath pathB
    restAsB = filter (not . null) . concatMap (splitOn pathB) $ restAsA
    pathC = getPart [] restAsB
    restAsC = filter (not . null) . concatMap (splitOn pathC) $ restAsB
    shrinkedPathC = shrinkPath pathC
    mkPr as
      | null as = []
      | pathA `isPrefixOf` as = A : (mkPr . drop (length pathA)) as
      | pathB `isPrefixOf` as = B : (mkPr . drop (length pathB)) as
      | pathC `isPrefixOf` as = C : (mkPr . drop (length pathC)) as
      | otherwise = []
    pr = mkPr actions

findPath :: [[Action]] -> [CPath]
findPath = map (\(Just cp) -> cp) . filter isJust . map convolutePath

startRobot :: Program -> Program
startRobot pr = head . map (\path -> execute . setMem 0 2 $ pr {pIn = map ord . show $ path}) $ paths
  where
    allPaths = findAllPaths . mkScaffold $ pr
    paths = findPath allPaths

part1Solution :: Program -> Int
part1Solution pr =
  sum . map (\(_, Point x y) -> x * y)
    . filter ((== 4) . fst)
    . map (\p -> (length . filter (`S.member` area) . pointsAround $ p, p))
    . S.elems
    $ area
  where
    scaffold = mkScaffold pr
    area = scMap scaffold

part2Solution :: Program -> String
part2Solution = (\ns -> (map chr . init) ns <> "\n" <> (show . last) ns) . getOut . startRobot
