{-# LANGUAGE LambdaCase #-}

module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import qualified Data.IntMap                  as M
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

data Rule = And [Rule] | Or [Rule] | Symb Char | Ref Int deriving (Show)

pRef :: ReadPrec Rule
pRef = Ref <$> readPrec

pSymb :: ReadPrec Rule
pSymb = Symb . head <$> readPrec

pOr :: ReadPrec Rule
pOr = do
    r <- pAnd <|> pRef <|> pSymb
    lift P.skipSpaces
    lift $ P.char '|'
    lift P.skipSpaces
    rest <- pOr <|> pAnd <|> pSymb <|> pRef
    case rest of
      r'@(And _)  -> return (Or [r, r'])
      (Or rs)     -> return (Or (r:rs))
      r'@(Symb _) -> return (Or [r, r'])
      r'@(Ref _)  -> return (Or [r, r'])

pAnd :: ReadPrec Rule
pAnd = do
    r <- pSymb <|> pRef
    lift P.skipSpaces
    rest <- pAnd <|> pRef <|> pSymb
    case rest of
      (And rs)    -> return (And (r:rs))
      r'@(Symb _) -> return (And [r, r'])
      r'@(Ref _)  -> return (And [r, r'])

instance Read Rule where
    readPrec = pSymb <++ pOr <++ pAnd <++ pRef

data Line = RuleLine Int Rule | MessageLine String deriving (Show)

pRuleLine :: ReadPrec Line
pRuleLine = do
        n <- readPrec
        lift $ P.char ':'
        lift P.skipSpaces
        RuleLine n <$> readPrec

pMessageLine :: ReadPrec Line
pMessageLine = MessageLine <$> lift (P.many1 P.get)

instance Read Line where
    readPrec = pRuleLine <++ pMessageLine

type Rules = M.IntMap Rule

input :: IO (Rules, [String])
input = do
    mls <- map read . filter (not . null) . lines <$> readFile "input"
    let rules = M.fromList . map (\(RuleLine n r) -> (n, r))
              .filter (\case (RuleLine _ _) -> True
                             _ -> False) $ mls
        message = map (\(MessageLine cs) -> cs) . filter (\case (MessageLine _) -> True
                                                                _ -> False) $ mls
    return (rules, message)

match :: Rules -> Int -> String -> [String]
match rs n cs = case n `M.lookup` rs of
                  Just r -> matchRule rs r cs
                  _      -> []

matchRule :: Rules -> Rule -> String -> [String]
matchRule rs (And rs') cs = foldl (\css r -> concatMap (matchRule rs r) css) [cs] rs'
matchRule rs (Or rs') cs = concatMap (\r -> matchRule rs r cs) rs'
matchRule rs (Symb c) "" = []
matchRule rs (Symb c) cs
  | c == head cs = [tail cs]
  | otherwise = []
matchRule rs (Ref n) cs = match rs n cs

part1solution :: IO ()
part1solution = do
    (rs, css) <- input
    print . length . filter (any null . match rs 0) $ css

part2solution :: IO ()
part2solution = do
    (rs, css) <- input
    let rs' = M.insert 8 (read "42 | 42 8") . M.insert 11 (read "42 31 | 42 11 31") $ rs
    print . length . filter (any null . match rs' 0) $ css
