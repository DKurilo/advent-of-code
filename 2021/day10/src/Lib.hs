module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (sort)

scoreErr :: String -> Int
scoreErr ")" = 3
scoreErr "]" = 57
scoreErr "}" = 1197
scoreErr ">" = 25137
scoreErr _ = 0

scoreAutocpmplete :: Char -> Int
scoreAutocpmplete ')' = 1
scoreAutocpmplete ']' = 2
scoreAutocpmplete '}' = 3
scoreAutocpmplete '>' = 4
scoreAutocpmplete _ = 0

pair :: Char -> Char
pair '(' = ')'
pair '[' = ']'
pair '{' = '}'
pair '<' = '>'
pair c = c

closes :: Char -> Char -> Bool
closes c c' = c == pair c'

parse :: String -> (String, String)
parse = doer ""
  where
    doer :: String -> String -> (String, String)
    doer parsed [] = (reverse parsed, "")
    doer parsed (c : cs)
      | c == '(' || c == '[' || c == '{' || c == '<' = doer (c : parsed) cs
      | c == ')' || c == ']' || c == '}' || c == '>' = close c parsed cs
      | otherwise = (reverse parsed, [c])
    close :: Char -> String -> String -> (String, String)
    close c [] cs = ("", [c])
    close c (c' : parsed) cs
      | c `closes` c' = doer parsed cs
      | otherwise = (reverse (c' : parsed), [c])

autocomplete :: String -> String
autocomplete = reverse . map pair

part1Solution :: [String] -> Int
part1Solution = sum . map (scoreErr . snd . parse)

part2Solution :: [String] -> Int
part2Solution cs = scores !! middle
  where
    scores =
      sort
        . map (foldl (\score c -> score * 5 + scoreAutocpmplete c) 0 . autocomplete . fst)
        . filter (null . snd)
        . map parse
        $ cs
    middle = length scores `div` 2
