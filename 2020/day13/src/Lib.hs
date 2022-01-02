module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import           Data.List.Split              (splitOn)
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

data Bus = B Int | X deriving (Eq, Show)

instance Read Bus where
   readPrec =
       (B <$> readPrec) <|> lift (do
        P.char 'x'
        return X)

input :: IO (Int, [Bus])
input = do
    (time:buses:_) <- lines <$> readFile "input"
    return (read time, (map read . splitOn ",") buses)

part1solution :: IO ()
part1solution = do
    (time, buses) <- input
    let times = (map (\(B b) -> (b * (time `div` b) + if time `mod` b == 0 then 0 else b , b)) . filter (/=X)) buses
    print . (\(t, b) -> (t - time)  * b) . minimum $ times

-- it solves set of equations like
-- k0 + x0 * n0 = k1 + x1 * n1 = k2 + x2 * n2 = ... = k(i) + x(i) * n(i)
-- It works using idea:
-- if (x * n0) `mod` d == r for some smallest n0 and next time (x * n1) `mod` d == r for n1 then
-- ∀ n such as (x * n) `mod` d ≡ r can be exressed as n = n0 + k * (n1 - n0) where k is any natural number.
-- It's not so difficult to prove this.
-- So for each pair (x0, x(i)) I can find n0 and n1 and to repeat search for new sequence that is one part less then previous.
-- This way when I have only one member, I have solution.
-- P.S. well, now I know it's related to Chinese reminder theorem! and it's possible to solve this problem faster.
findSolution :: [(Int, Int)] -> Int
findSolution [] = error "wrong data"
findSolution [(k, _)] = k
findSolution ((k, x): kxs) = k + x * ( findSolution
                                     . map (\(k', x') -> ( (\((k'', _):(x'', _):_) -> (k'', x'' - k''))
                                                         . filter snd
                                                         . map (\i -> (i, (i * x + k - k') `mod` x' == 0))) [0..])) kxs

part2solution :: IO ()
part2solution = do
    (_, buses) <- input
    let shift = (length . takeWhile (==X)) buses
        xs = map (\(i, B b) -> (-i, b)) . filter (\(_, b) -> b /= X) .  zip [0..] . drop shift $ buses
    (print . (\y -> y - fromIntegral shift) . findSolution) xs
