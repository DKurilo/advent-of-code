module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.Char (isAlphaNum)
import Data.List (find, foldl', intercalate, sortOn)
import Data.Ord (Down(Down))
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Op
  = AND
  | OR
  | XOR
  deriving (Show)

instance Read Op where
  readPrec =
    lift
      $ (do
           _ <- P.string "AND"
           return AND)
          P.<++ (do
                   _ <- P.string "OR"
                   return OR)
          P.<++ (do
                   _ <- P.string "XOR"
                   return XOR)

applyOp :: Op -> Bool -> Bool -> Bool
applyOp AND x1 x2 = x1 && x2
applyOp OR x1 x2 = x1 || x2
applyOp XOR x1 x2 = x1 /= x2

data Signal
  = T
  | F
  | S String
  deriving (Show, Eq)

instance Read Signal where
  readPrec = lift $ S <$> P.munch1 isAlphaNum

data Gate = Gate
  { gop :: Op
  , gi1 :: Signal
  , gi2 :: Signal
  , go :: String
  } deriving (Show)

instance Read Gate where
  readPrec = do
    s1 <- readPrec
    lift P.skipSpaces
    op <- readPrec
    lift P.skipSpaces
    s2 <- readPrec
    lift P.skipSpaces
    _ <- lift . P.string $ "->"
    lift P.skipSpaces
    fmap (Gate op s1 s2) . lift . P.munch1 $ isAlphaNum

data Wire = Wire
  { wname :: String
  , wval :: Bool
  } deriving (Show)

instance Read Wire where
  readPrec =
    lift $ do
      name <- P.munch1 isAlphaNum
      _ <- P.char ':'
      P.skipSpaces
      val <- P.char '1' P.<++ P.char '0'
      return $ Wire name (val == '1')

data Propagator = Propagator
  { pStopped :: Bool
  , pWires :: [Wire]
  , pGates :: [Gate]
  } deriving (Show)

readWire :: P.ReadP Wire
readWire = do
  w <- PC.readPrec_to_P readPrec 0
  P.skipSpaces
  return w

readGate :: P.ReadP Gate
readGate = do
  g <- PC.readPrec_to_P readPrec 0
  P.skipSpaces
  return g

instance Read Propagator where
  readPrec =
    lift $ do
      ws <- P.many1 readWire
      gs <- P.many1 readGate
      return $ Propagator False ws gs

evalPropagator :: Propagator -> Propagator
evalPropagator p
  | pStopped p = p
  | otherwise = Propagator (null wires) (pWires p <> wires) gates
  where
    (gates, wires) = foldl' doer ([], []) . pGates $ p
    doer :: ([Gate], [Wire]) -> Gate -> ([Gate], [Wire])
    doer (gs, ws) g =
      case (s1, s2) of
        (Just x1, Just x2) -> (gs, Wire (go g) (applyOp (gop g) x1 x2) : ws)
        _ -> (g : gs, ws)
      where
        s1 =
          case gi1 g of
            S cs -> fmap wval . find ((== cs) . wname) . pWires $ p
            T -> Just True
            F -> Just False
        s2 =
          case gi2 g of
            S cs -> fmap wval . find ((== cs) . wname) . pWires $ p
            T -> Just True
            F -> Just False

runPropagator :: Propagator -> Propagator
runPropagator = head . dropWhile (not . pStopped) . iterate evalPropagator

bitToInt :: [Bool] -> Int
bitToInt = doer 0
  where
    doer :: Int -> [Bool] -> Int
    doer x [] = x
    doer x (b:bs') =
      doer
        (x * 2
           + (if b
                then 1
                else 0))
        bs'

getOutput :: Propagator -> Int
getOutput =
  bitToInt
    . fmap wval
    . sortOn (Down . wname)
    . filter ((== 'z') . head . wname)
    . pWires

getOutput' :: Propagator -> [Bool]
getOutput' =
  fmap wval . sortOn (Down . wname) . filter ((== 'z') . head . wname) . pWires

showOutput :: [Bool] -> String
showOutput =
  fmap
    (\b ->
       if b
         then '#'
         else '.')

part1Solution :: String -> Int
part1Solution = getOutput . runPropagator . read

variance :: Int -> [a] -> [[a]]
variance 0 _ = [[]]
variance _ [] = []
variance n (x:xs) = (fmap (x :) . variance (n - 1)) xs <> variance n xs

swapOutputs :: Int -> Int -> Propagator -> Propagator
swapOutputs i k p = p {pGates = gs'}
  where
    gs = pGates p
    gi = gs !! i
    gk = gs !! k
    gs' =
      fmap
        (\(n, g) ->
           if n == i
             then g {go = go gk}
             else if n == k
                    then g {go = go gi}
                    else g)
        . zip [0 ..]
        $ gs

part2Solution :: String -> String
part2Solution cs =
  intercalate
    "\n"
    [ show (i + 10) <> ": " <> (showOutput . getOutput' . runPropagator) pr'
    | i <- [0 .. (length . pWires) pr - 1]
    , let pr' =
            pr
              { pWires =
                  zipWith
                    (\j w ->
                       if j == i
                         then w {wval = True}
                         else w)
                    [0 ..]
                    . pWires
                    $ pr
              }
    ]
  where
    pr =
      (\p -> p {pWires = fmap (\w -> w {wval = False}) . pWires $ p}) . read
        $ cs
-- After analyzing input I found this problems:
-- x08 AND y08 -> gvw
-- x08 XOR y08 -> qjb
-- 
-- y15 AND x15 -> z15
-- fbv XOR rgt -> jgc
-- 
-- hwm AND tdc -> z22
-- tdc XOR hwm -> drg
-- 
-- qrg OR ppf -> z35
-- vcs XOR dtj -> jbp
