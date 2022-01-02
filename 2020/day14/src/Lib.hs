module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import qualified Data.Map                     as M
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

-- String is inverted sequence of bits (or X for mask)
data Op = Mem Int String | Mask String deriving (Eq, Show)

readLet :: ReadPrec ()
readLet = lift $ do
    P.skipSpaces
    P.char '='
    P.skipSpaces
    return ()

instance Read Op where
    readPrec = (do
        lift $ P.string "mem["
        address <- readPrec
        lift $ P.char ']'
        readLet
        Mem address . intToValue <$> readPrec) <|> (do
        lift $ P.string "mask"
        readLet
        mask <- lift $ P.munch1 (\c -> c == '1' || c == '0' || c == 'X')
        return . Mask . reverse $ mask)

type Program = [Op]

data State = St (M.Map Int String) String

mkState :: State
mkState = St M.empty (replicate 36 'X')

snapshot :: State -> M.Map Int String
snapshot (St vs _) = vs

applyMask :: String -> String -> String
applyMask mask = zipWith (\m v -> case m of
                                    '1' -> '1'
                                    '0' -> '0'
                                    _   -> v) mask . (++ repeat '0')

intToValue :: Int -> String
intToValue n = fst . foldl (\(v, n') _ -> (v ++ show (n' `mod` 2), n' `div` 2)) ("", n) $ [0..36]

valueToInt :: String -> Int
valueToInt = foldr (\v n -> n * 2  + if v == '0' then 0 else 1) 0

execute1 :: Program -> M.Map Int Int
execute1 = M.map valueToInt . snapshot . foldl apply mkState
    where apply :: State -> Op -> State
          apply (St vs mask) (Mem address value) = St (M.insert address (applyMask mask value) vs) mask
          apply (St vs _) (Mask mask) = St vs mask

unmask :: String -> [String]
unmask [] = error "wrong value"
unmask "X" = ["0", "1"]
unmask [c] = [[c]]
unmask ('X':cs) = map ('0':) vs ++ map ('1':) vs
    where vs = unmask cs
unmask (c:cs) = map (c:) . unmask $ cs

applyMaskToAddress :: String -> Int -> [Int]
applyMaskToAddress mask = map valueToInt . unmask . zipWith (\m v -> case m of
                                                                       '1' -> '1'
                                                                       '0' -> v
                                                                       _   -> 'X') mask . (++ repeat '0') . intToValue

execute2 :: Program -> M.Map Int Int
execute2 = M.map valueToInt . snapshot . foldl apply mkState
    where apply :: State -> Op -> State
          apply (St vs mask) (Mem address value) = St (foldl (\vs' a -> M.insert a value vs') vs . applyMaskToAddress mask $ address) mask
          apply (St vs _) (Mask mask) = St vs mask

input :: IO Program
input = map read . filter (not . null) . lines <$> readFile "input"

part1solution :: IO ()
part1solution = print . M.foldl (+) 0 . execute1 =<< input

part2solution :: IO ()
part2solution = print . M.foldl (+) 0 . execute2 =<< input
