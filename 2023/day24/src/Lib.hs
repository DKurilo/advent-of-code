{-# LANGUAGE TupleSections #-}

module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Data.Either (fromRight)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Matrix (fromLists, rref, zero, (!))
import Data.Maybe (fromJust, fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read
import Z3.Monad

data Point = P {px :: Rational, py :: Rational, pz :: Rational} deriving (Show, Eq, Ord)

readDivider :: ReadPrec ()
readDivider = lift $ do
  P.skipSpaces
  _ <- P.char ','
  P.skipSpaces

instance Read Point where
  readPrec = do
    x <- readPrec
    readDivider
    y <- readPrec
    readDivider
    P (fromInteger x) (fromInteger y) . fromInteger <$> readPrec

setZToP :: Rational -> Point -> Point
setZToP z p = p {pz = z}

data Hailstone = HS {hp1 :: Point, hp2 :: Point, hv :: Point} deriving (Show)

instance Read Hailstone where
  readPrec = do
    p1 <- readPrec
    lift P.skipSpaces
    _ <- lift . P.char $ '@'
    lift P.skipSpaces
    vs <- readPrec
    return (HS p1 (P (px p1 + px vs) (py p1 + py vs) (pz p1 + pz vs)) vs)

setZToHailstone :: Rational -> Hailstone -> Hailstone
setZToHailstone z (HS p1 p2 vs) = HS (setZToP z p1) (setZToP z p2) (vs {pz = 0})

-- Well, it was stupid... I need to sleep more...
areTheSame :: Hailstone -> Hailstone -> Bool
areTheSame h1 h2
  | dvxy /= 0 || dvxz /= 0 = False
  | dx1 == 0 && dx2 == 0 && uy == uz = True
  | dx1 == 0 && uy == uz = False
  | dy1 == 0 && dy2 == 0 && ux == uz = True
  | dy1 == 0 && ux == uz = False
  | dz1 == 0 && dz2 == 0 && ux == uy = True
  | dz1 == 0 && ux == uy = False
  | ux == uy && uy == uz = True
  | otherwise = False
  where
    (HS (P x1 y1 z1) (P x2 y2 z2) _) = h1
    (HS (P x3 y3 z3) (P x4 y4 z4) _) = h2
    dx1 = x1 - x2
    dy1 = y1 - y2
    dz1 = z1 - z2
    dx2 = x3 - x4
    dy2 = y3 - y4
    dz2 = z3 - z4
    dvxy = dx1 * dy2 - dy1 * dx2
    dvxz = dx1 * dz2 - dz1 * dx2
    ux = (x1 - x3) / dx1
    uy = (y1 - y3) / dy1
    uz = (z1 - z3) / dz1

areIntersectedPathes :: Hailstone -> Hailstone -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Bool
areIntersectedPathes h1 h2 fromX toX fromY toY fromZ toZ
  | areTheSame h1 h2 = True
  | dvxy == 0 && dvxz == 0 = False
  | dvxz == 0 && z1 == z2 && z2 == z3 && z3 == z4 && x >= fromX && x <= toX && y >= fromY && y <= toY =
      t1x >= 0 && t2x >= 0 && t1y >= 0 && t2y >= 0
  | dvxy == 0 && x1 == x2 && x2 == x3 && x3 == x4 && z >= fromZ && z <= toZ && y >= fromY && y <= toY =
      t1z >= 0 && t2z >= 0
  | dvxy == 0 && y1 == y2 && y2 == y3 && y3 == y4 && z >= fromZ && z <= toZ && x >= fromX && x <= toX =
      t1z >= 0 && t2z >= 0
  | dvxy == 0 || dvxz == 0 = False
  | x >= fromX && x <= toX && y >= fromY && y <= toY && z >= fromZ && z <= toZ =
      t1z >= 0 && t2x >= 0 && t1y >= 0 && t2y >= 0 && t1z >= 0 && t2z >= 0
  | otherwise = False
  where
    (HS (P x1 y1 z1) (P x2 y2 z2) (P v1x v1y v1z)) = h1
    (HS (P x3 y3 z3) (P x4 y4 z4) (P v2x v2y v2z)) = h2
    dx1 = x1 - x2
    dy1 = y1 - y2
    dz1 = z1 - z2
    dx2 = x3 - x4
    dy2 = y3 - y4
    dz2 = z3 - z4
    dvxy = dx1 * dy2 - dy1 * dx2
    dvxz = dx1 * dz2 - dz1 * dx2
    x = ((x1 * y2 - y1 * x2) * dx2 - dx1 * (x3 * y4 - y3 * x4)) / dvxy
    y = ((x1 * y2 - y1 * x2) * dy2 - dy1 * (x3 * y4 - y3 * x4)) / dvxy
    z = ((x1 * z2 - z1 * x2) * dz2 - dz1 * (x3 * z4 - z3 * x4)) / dvxz
    t1x = (x - x1) / v1x
    t2x = (x - x3) / v2x
    t1y = (y - y1) / v1y
    t2y = (y - y3) / v2y
    t1z = (z - z1) / v1z
    t2z = (z - z3) / v2z

areCollide :: Hailstone -> Hailstone -> Rational -> Rational -> Rational -> Rational -> Rational -> Rational -> Bool
areCollide h1 h2 fromX toX fromY toY fromZ toZ =
  ( vx1 == vx2
      && x1 == x2
      && x1 >= fromX
      && x1 <= toX
      && vy1 == vy2
      && y1 == y2
      && y1 >= fromY
      && y1 <= toY
      && vz1 == vz2
      && z1 == z2
      && z1 >= fromZ
      && z1 <= toZ
  )
    || ( vx1 == vx2
           && x1 == x2
           && x1 >= fromX
           && x1 <= toX
           && vy1 == vy2
           && y1 == y2
           && y1 >= fromY
           && y1 <= toY
           && vz1 /= vz2
           && tz >= 0
           && z >= fromZ
           && z <= toZ
       )
    || ( vx1 == vx2
           && x1 == x2
           && x1 >= fromX
           && x1 <= toX
           && vy1 /= vy2
           && ty >= 0
           && y >= fromY
           && y <= toY
           && vz1 == vz2
           && z1 == z2
           && z1 >= fromZ
           && z1 <= toZ
       )
    || ( vx1 /= vx2
           && tx >= 0
           && x >= fromX
           && x <= toX
           && vy1 == vy2
           && y1 == y2
           && y1 >= fromY
           && y1 <= toY
           && vz1 == vz2
           && z1 == z2
           && z1 >= fromZ
           && z1 <= toZ
       )
    || ( vx1 == vx2
           && x1 == x2
           && x1 >= fromX
           && x1 <= toX
           && vy1 /= vy2
           && vz1 /= vz2
           && ty == tz
           && ty >= 0
           && y >= fromY
           && y <= toY
           && z >= fromZ
           && z <= toZ
       )
    || ( vy1 == vy2
           && y1 == y2
           && y1 >= fromY
           && y1 <= toY
           && vx1 /= vx2
           && vz1 /= vz2
           && tx == tz
           && tx >= 0
           && x >= fromX
           && x <= toX
           && z >= fromZ
           && z <= toZ
       )
    || ( vz1 == vz2
           && z1 == z2
           && z1 >= fromZ
           && z1 <= toZ
           && vx1 /= vx2
           && vy1 /= vy2
           && tx == ty
           && tx >= 0
           && x >= fromX
           && x <= toX
           && y >= fromY
           && y <= toY
       )
    || ( vx1 /= vx2
           && vy1 /= vy2
           && vz1 /= vz2
           && tx == ty
           && tz == tx
           && tx >= 0
           && x >= fromX
           && x <= toX
           && y >= fromY
           && y <= toY
           && z >= fromZ
           && z <= toZ
       )
  where
    (HS (P x1 y1 z1) _ (P vx1 vy1 vz1)) = h1
    (HS (P x2 y2 z2) _ (P vx2 vy2 vz2)) = h2
    (tx, x) = tc x1 vx1 x2 vx2
    (ty, y) = tc y1 vy1 y2 vy2
    (tz, z) = tc z1 vz1 z2 vz2

    tc c1 vc1 c2 vc2 = ((c1 - c2) / (vc2 - vc1), (vc2 * c1 - vc1 * c2) / (vc2 - vc1))

part1Solution :: String -> Int
part1Solution cs =
  length
    [ ()
      | i <- [0 .. lastHs - 1],
        j <- [i + 1 .. lastHs],
        areIntersectedPathes (hs !! i) (hs !! j) from to from to 0 0
    ]
  where
    css = splitOn "\n\n" cs
    hs =
      ( fmap (setZToHailstone 0 . read)
          . lines
          . head
      )
        css
    lastHs = length hs - 1
    (fromI, toI) = read . last $ css
    from = fromInteger fromI
    to = fromInteger toI

data Term
  = Const Rational
  | Var String
  | Mul Term Term
  | Div Term Term
  | Add Term Term
  deriving (Show, Eq, Ord)

-- Figure out this solution after I solved it with z3 solver.
-- But it can be the best solution here.
-- idea is:
-- colliding with hailstone 0:
-- xr + vrx * t0 = x0 + v0x * t0 => t0 = (x0 - xr) / (vrx - v0x)
-- yr + vry * t0 = y0 + v0y * t0 => t0 = (y0 - yr) / (vry - v0y)
-- zr + vrz * t0 = z0 + v0z * t0
-- so from first two equasions:
-- x0 * vry - x0 * v0y - xr * vry + xr * v0y = y0 * vrx - y0 * v0x - yr * vrx + yr * v0x
-- and from this:
-- yr * vrx - xr * vry = y0 * vrx - y0 * v0x + yr * v0x - x0 * vry + x0 * v0y - xr * v0y
-- the same we can do with hailstone 1. And then substract them.
-- xr * (v1y - v0y) + vrx * (y0 - y1) + yr * (v0x - v1x) + vry * (x1 - x0) = y0 * v0x - x0 * v0y - y1 * v1x + x1 * v1y
-- so from 5 first hailstones we can build matrix that give us solutions for x and y [(0, 1), (0, 2), (0, 3), (0, 4)]
-- then we can build matrix to find z:
-- zr + vrz * t0 = z0 + v0z * t0
-- zr + vrz * t1 = z1 + v1z * t1
part2Solution :: String -> Rational
part2Solution cs = xr + yr + zr
  where
    hs =
      ( fmap read
          . lines
          . head
      )
        . splitOn "\n\n"
        $ cs
    xySolutionMx =
      fromRight (zero 4 5)
        . rref
        . fromLists
        . fmap
          ( \hn ->
              let (P xn yn _) = hp1 hn
                  (P vnx vny _) = hv hn
               in [vny - v0y, y0 - yn, v0x - vnx, xn - x0, y0 * v0x - x0 * v0y - yn * vnx + xn * vny]
          )
        . take 4
        . tail
        $ hs
    xr = xySolutionMx ! (1, 5)
    vrx = xySolutionMx ! (2, 5)
    yr = xySolutionMx ! (3, 5)
    h0 = head hs
    (P x0 y0 z0) = hp1 h0
    (P v0x v0y v0z) = hv h0
    (P x1 _ z1) = hp1 h1
    (P v1x _ v1z) = hv h1
    h1 = hs !! 1
    t0 = (x0 - xr) / (vrx - v0x)
    t1 = (x1 - xr) / (vrx - v1x)
    zSolutionMx =
      fromRight (zero 2 3) . rref . fromLists $
        [ [1, t0, z0 + v0z * t0],
          [1, t1, z1 + v1z * t1]
        ]
    zr = zSolutionMx ! (1, 3)

-- next solution I did first after tried to build my own solver in 24 hours that I had.
-- I didn't finish my solver. Maybe I'll try to do this at some point later.
-- But I start checking if there are some ready to use solver and found z3 and haskell library for it.
mkTask :: [Hailstone] -> Z3 (Maybe Integer)
mkTask hs = do
  xr <- mkFreshIntVar "xr"
  yr <- mkFreshIntVar "yr"
  zr <- mkFreshIntVar "zr"
  vrx <- mkFreshIntVar "vrx"
  vry <- mkFreshIntVar "vry"
  vrz <- mkFreshIntVar "vrz"

  forM_ (zip [0 :: Int ..] hs) $ \(i, HS (P x y z) _ (P vx vy vz)) -> do
    ti <- mkFreshIntVar ("t" <> show i)

    xi <- mkInteger . ceiling $ x
    yi <- mkInteger . ceiling $ y
    zi <- mkInteger . ceiling $ z
    vxi <- mkInteger . ceiling $ vx
    vyi <- mkInteger . ceiling $ vy
    vzi <- mkInteger . ceiling $ vz

    mxli <- mkMul [vxi, ti]
    lhsXi <- mkAdd [xi, mxli]
    mxri <- mkMul [vrx, ti]
    rhsXi <- mkAdd [xr, mxri]
    assert =<< mkEq lhsXi rhsXi

    myli <- mkMul [vyi, ti]
    lhsYi <- mkAdd [yi, myli]
    myri <- mkMul [vry, ti]
    rhsYi <- mkAdd [yr, myri]
    assert =<< mkEq lhsYi rhsYi

    mzli <- mkMul [vzi, ti]
    lhsZi <- mkAdd [zi, mzli]
    mzri <- mkMul [vrz, ti]
    rhsZi <- mkAdd [zr, mzri]
    assert =<< mkEq lhsZi rhsZi

  toFind <- mkAdd [xr, yr, zr]
  snd <$> withModel (\m -> fromJust <$> evalInt m toFind)

-- it's easy to understand
-- the problem is it uses z3 solver and for some reason it doesn't work well
-- in nix with darwin.
-- but all complexity is in z3 solver.
part2Solution_ :: String -> IO Integer
part2Solution_ cs = do
  let css = splitOn "\n\n" cs
      hs =
        ( fmap read
            . lines
            . head
        )
          css
  result <- evalZ3 . mkTask . take 3 $ hs
  return . fromMaybe 0 $ result

-- stop here
-- next lines are mess...
data EquasionSystem = EQS {eqsEqs :: [Term], eqsVars :: M.Map String Term} deriving (Show)

mkEquasionSystem :: EquasionSystem
mkEquasionSystem = EQS [] M.empty

addHailstoneToEquasionSystem :: EquasionSystem -> Int -> Hailstone -> EquasionSystem
addHailstoneToEquasionSystem eqs n h = eqs {eqsEqs = heailstoneToTerms n h <> eqsEqs eqs}

heailstoneToTerms :: Int -> Hailstone -> [Term]
heailstoneToTerms n (HS (P x y z) _ (P vx vy vz)) = [term "x" x vx, term "y" y vy, term "z" z vz]
  where
    var pr = Var (pr <> show n)
    term ax r vr =
      Add
        (Const r)
        (Add (Mul (Const vr) (var "t")) (Mul (Const (-1)) (Add (Var (ax <> "r")) (Mul (Var ("v" <> ax <> "r")) (var "t")))))

unionVarsWith :: (Int -> Int -> Int) -> (M.Map String Int -> String -> Int -> Bool) -> Term -> Term -> [(String, Int)]
unionVarsWith fu ff tl tr = M.toList . M.filterWithKey (ff rvars) . M.unionWith fu lvars $ rvars
  where
    lvars = getvars tl
    rvars = getvars tr

    getvars = M.fromListWith max . varsInTerm

varsInTerm :: Term -> [(String, Int)]
varsInTerm (Const _) = []
varsInTerm (Var cs) = [(cs, 1)]
varsInTerm (Mul tl tr) = unionVarsWith (+) (\_ _ -> const True) tl tr
varsInTerm (Div tl tr) = unionVarsWith (-) (\m k _ -> not (k `M.member` m)) tl tr
varsInTerm (Add tl tr) = unionVarsWith max (\_ _ -> const True) tl tr

subst :: String -> Term -> Term -> Term
subst _ _ t'@(Const _) = t'
subst name t t'@(Var cs)
  | name == cs = t
  | otherwise = t'
subst name t (Mul tl tr) = Mul (subst name t tl) (subst name t tr)
subst name t (Div tl tr) = Div (subst name t tl) (subst name t tr)
subst name t (Add tl tr) = Add (subst name t tl) (subst name t tr)

evalTerm :: Term -> Term
evalTerm term
  | term == t' = term
  | otherwise = evalTerm t'
  where
    t' = doer term

    doer :: Term -> Term
    doer t@(Const _) = t
    doer t@(Var _) = t
    doer (Mul tl (Const 1)) = tl
    doer (Mul (Const 1) tr) = tr
    doer (Mul _ (Const 0)) = Const 0
    doer (Mul (Const 0) _) = Const 0
    doer (Mul tl tr) = case (doer tl, doer tr) of
      (Const xl, Const xr) -> Const (xl * xr)
      (tl', tr') -> Mul tl' tr'
    doer (Div tl (Const 1)) = tl
    doer (Div tl tr) = case (doer tl, doer tr) of
      (Const 0, _) -> Const 0
      (_, Const 0) -> error "divided by zero"
      (Const xl, Const xr) -> Const (xl / xr)
      (tl', tr') -> Div tl' tr'
    doer (Add (Const 0) tr) = tr
    doer (Add tl (Const 0)) = tl
    doer (Add tl tr) = case (doer tl, doer tr) of
      (Const xl, Const xr) -> Const (xl + xr)
      (tl', tr') -> Add tl' tr'

noAddDiv :: Term -> Bool
noAddDiv (Const _) = True
noAddDiv (Var _) = True
noAddDiv (Add _ _) = False
noAddDiv (Mul t1 t2) = noAddDiv t1 && noAddDiv t2
noAddDiv (Div _ _) = False

noAdd :: Term -> Bool
noAdd (Const _) = True
noAdd (Var _) = True
noAdd (Add _ _) = False
noAdd (Mul t1 t2) = noAdd t1 && noAdd t2
noAdd (Div t1 _) = noAdd t1

nonSimplifiable :: Term -> Bool
nonSimplifiable (Const _) = True
nonSimplifiable (Var _) = True
nonSimplifiable (Add _ _) = False
nonSimplifiable (Mul t1 t2) = nonSimplifiable t1 && nonSimplifiable t2
nonSimplifiable (Div (Div _ _) _) = False
nonSimplifiable (Div _ (Div _ _)) = False
nonSimplifiable (Div t1 _) = nonSimplifiable t1

goodRest :: Term -> Bool
goodRest (Add t1 t2) = nonSimplifiable t1 && goodRest t2
goodRest t@(Mul _ _) = nonSimplifiable t
goodRest (Div t _) = nonSimplifiable t -- We are leaving divider as is for now
goodRest _ = True

isNormal :: Term -> Bool
isNormal (Add t1 t2) = noAddDiv t1 && isNormal t2
isNormal (Mul t1 t2) = noAddDiv t1 && noAddDiv t2
isNormal (Div _ _) = False
isNormal _ = True

step1 :: Term -> Term
step1 t@(Const _) = t
step1 t@(Var _) = t
step1 (Add (Add t1 t2) t3) = step1 $ Add (step1 t1) (Add (step1 t2) (step1 t3))
step1 (Mul (Mul t1 t2) t3) = step1 $ Mul (step1 t1) (Mul (step1 t2) (step1 t3))
step1 (Div (Div t1 t2) t3) = step1 $ Div (step1 t1) (Mul (step1 t2) (step1 t3))
step1 (Div t1 (Div t2 t3)) = step1 $ Div (Mul (step1 t1) (step1 t3)) (step1 t2)
step1 (Mul (Add t1 t2) t3) = step1 (Add (Mul t1' t3') (Mul t2' t3'))
  where
    t1' = step1 t1
    t2' = step1 t2
    t3' = step1 t3
step1 (Mul t1 t2@(Add _ _)) = step1 (Mul t2 t1)
step1 t@(Mul t1 t2)
  | noAdd t1 && noAdd t2 = t
  | otherwise = step1 (Mul t1' t2')
  where
    t1' = step1 t1
    t2' = step1 t2
step1 t@(Div (Add t1 t2) t3) = trace ("divAdd " <> show t) $ step1 (Add (Div (step1 t1) t3) (Div (step1 t2) t3))
step1 t@(Div t1 t2)
  | noAdd t1 = trace ("div 1 " <> show t) t
  | otherwise = trace ("div 2 " <> show t) $ step1 (Div (step1 t1) t2)
step1 t@(Add t1 t2)
  | goodt1 && goodt2 = t
  | goodt1 = trace ("add good t1 " <> show t) $ step1 (Add t1 (step1 t2))
  | goodt2 = trace ("add good t2 " <> show t) $ step1 (Add (step1 t1) t2)
  | otherwise = step1 (Add (step1 t1) (step1 t2))
  where
    goodt1 = nonSimplifiable t1
    goodt2 = goodRest t2

step3 :: Term -> Term
step3 t
  | null allDivsInTerm = t
  | otherwise = applyDiv (head allDivsInTerm) t
  where
    allDivsInTerm = allDivs t

    allDivs :: Term -> [Term]
    allDivs (Add t1 t2) = allDivs t1 <> allDivs t2
    allDivs (Mul t1 t2) = allDivs t1 <> allDivs t2
    allDivs (Div _ t1) = [t1]
    allDivs _ = []

    applyDiv :: Term -> Term -> Term
    applyDiv mt (Add t1 t2) = Add (applyDiv mt t1) (applyDiv mt t2)
    applyDiv mt t'@(Mul t1 t2) = case applyMulDiv mt t1 of
      Just t1' -> Mul t1' t2
      _ -> case applyMulDiv mt t2 of
        Just t2' -> Mul t1 t2'
        _ -> Mul mt t'
    applyDiv mt (Div t1 t2)
      | mt == t2 = t1
      | otherwise = case applyDivDiv mt t1 of
          Just t1' -> Div t1' t2
          _ -> Mul mt (Div t1 t2)
    applyDiv mt t1 = Mul mt t1

    applyMulDiv :: Term -> Term -> Maybe Term
    applyMulDiv mt (Div t1 t2)
      | mt == t2 = Just t1
      | otherwise = case applyDivDiv mt t1 of
          Just t1' -> Just (Div t1' t2)
          _ -> Nothing
    applyMulDiv mt (Mul t1 t2) = case applyMulDiv mt t1 of
      Just t1' -> Just $ Mul t1' t2
      _ -> case applyMulDiv mt t2 of
        Just t2' -> Just $ Mul t1 t2'
        _ -> Nothing
    applyMulDiv _ _ = Nothing

    applyDivDiv :: Term -> Term -> Maybe Term
    applyDivDiv mt (Div t1 t2)
      | mt == t2 = Just t1
      | otherwise = case applyDivDiv mt t1 of
          Just t1' -> Just (Div t1' t2)
          _ -> Nothing
    applyDivDiv mt (Mul t1 t2) = case applyDivDiv mt t1 of
      Just t1' -> Just $ Mul t1' t2
      _ -> case applyDivDiv mt t2 of
        Just t2' -> Just $ Mul t1 t2'
        _ -> Nothing
    applyDivDiv _ _ = Nothing

normalizeTerm :: Term -> Term
normalizeTerm term
  | isNormal term = evalTerm term
  | otherwise = trace "normalizeTerm internal" $ normalizeTerm . evalTerm . step3 . step1 $ term

popVar :: String -> Term -> Term
popVar cs t = trace (show ("Pop", cs)) $ case doer t of
  Just t' -> case extractCoefs t' of
    (Just t'', Just rest) -> Add (Mul (Var cs) t'') rest
    (Just t'', Nothing) -> Mul (Var cs) t''
    _ -> t'
  _ -> t
  where
    doer :: Term -> Maybe Term
    doer (Add t1 t2) = case (doer t1, doer t2) of
      (Just t1', Just t2') -> Just (Add t1' t2')
      (Nothing, Just t2') -> Just $ addToAddEnd t1 t2'
      (Just t1', Nothing) -> Just $ Add t1' t2
      _ -> Nothing
    doer (Mul t1 t2) = case (doer t1, doer t2) of
      (Just t1', Just t2') -> Just (Mul t1' t2')
      (Nothing, Just t2') -> Just $ addToMulEnd t1 t2'
      (Just t1', Nothing) -> Just $ Mul t1' t2
      _ -> Nothing
    doer (Var cs')
      | cs == cs' = Just (Var cs')
      | otherwise = Nothing
    doer _ = Nothing

    extractCoefs :: Term -> (Maybe Term, Maybe Term)
    extractCoefs t'@(Mul (Var cs') t2)
      | cs' == cs = (Just t2, Nothing)
      | otherwise = (Nothing, Just t')
    extractCoefs t'@(Add t1 t2) = case (extractCoefs t1, extractCoefs t2) of
      ((Just t1', _), (Just t2', Just rest)) -> (Just (Add t1' t2'), Just rest)
      ((Just t1', _), (Nothing, Just rest)) -> (Just t1', Just rest)
      ((Just t1', _), (Just t2', Nothing)) -> (Just (Add t1' t2'), Nothing)
      ((Just t1', _), _) -> (Just t1', Nothing)
      ((Nothing, _), _) -> (Nothing, Just t')
    extractCoefs t' = (Nothing, Just t')

addToAddEnd :: Term -> Term -> Term
addToAddEnd t1 (Add t2 t3@(Add _ _)) = Add t2 (addToAddEnd t1 t3)
addToAddEnd t1 (Add t2 t3) = Add t2 (Add t3 t1)
addToAddEnd t1 t2 = Add t2 t1

addToMulEnd :: Term -> Term -> Term
addToMulEnd t1 (Mul t2 t3@(Mul _ _)) = Mul t2 (addToMulEnd t1 t3)
addToMulEnd t1 (Mul t2 t3) = Mul t2 (Mul t3 t1)
addToMulEnd t1 t2 = Mul t2 t1

express :: Term -> String -> Maybe Term
express t name = doer (popVar name . normalizeTerm . trace ("express " <> name <> " " <> show t) $ t) (Const 0)
  where
    doer :: Term -> Term -> Maybe Term
    doer (Const _) _ = Nothing
    doer (Var name') t'
      | name' == name = Just t'
      | otherwise = Nothing
    doer (Mul tl tr) t' = doer tl (Div t' tr) <|> doer tr (Div t' tl)
    doer (Add tl tr) t' = doer tl (Add t' (Mul (Const (-1)) tr)) <|> doer tr (Add t' (Mul (Const (-1)) tl))
    doer _ _ = Nothing

calculate :: M.Map String Term -> M.Map String Rational
calculate = M.map evalValue
  where
    evalValue t = case evalTerm t of
      Const x -> x
      _ -> -1

solve :: EquasionSystem -> M.Map String Rational
solve eqs
  | trace (show ("WTF", fmap snd varsTerms)) $ null varsTerms = trace ("no vars to substitute " <> show eqs) . calculate . eqsVars $ eqs
  | otherwise = trace (show ("WTF1", length eqs', M.size vars')) $ solve (EQS eqs' vars')
  where
    varsTerms = concatMap (\t'' -> (fmap ((t'',) . fst) . filter ((== 1) . snd) . varsInTerm) t'') . eqsEqs $ eqs
    (t, cs) = head varsTerms
    t' = case express (evalTerm t) cs of
      Just x -> x
      _ -> error ("can't express " <> cs <> " in " <> show t)
    eqs' = fmap (normalizeTerm . (\x -> trace ("eqs' " <> show x <> " from " <> show t <> " when substituted " <> cs <> " with " <> show t') x) . evalTerm . subst cs t') . filter (/= t) . eqsEqs $ eqs
    vars' = M.insert cs t' . M.map (normalizeTerm . (\x -> trace ("vars' " <> show x) x) . evalTerm . subst cs t') . eqsVars $ eqs

part2SolutionMaybeTryToFinish :: String -> Rational
part2SolutionMaybeTryToFinish cs = sum . fmap (fromMaybe (-1) . (`M.lookup` vars)) $ ["xr", "yr", "zr"]
  where
    css = splitOn "\n\n" cs
    hs =
      ( fmap (setZToHailstone 0 . read)
          . lines
          . head
      )
        css
    vars = solve . foldl' (uncurry . addHailstoneToEquasionSystem) mkEquasionSystem . zip [0 ..] $ hs
