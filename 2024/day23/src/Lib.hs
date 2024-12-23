module Lib
  ( part1Solution
  , part2Solution
  ) where

import Data.List (foldl', intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

readName :: P.ReadP String
readName = P.munch1 (/= '-')

data Conn = Conn
  { cc1 :: String
  , cc2 :: String
  } deriving (Show)

instance Read Conn where
  readPrec =
    lift $ do
      c1 <- readName
      _ <- P.char '-'
      Conn c1 <$> readName

allComps :: [Conn] -> S.Set String
allComps = S.fromList . concatMap (\c -> [cc1 c, cc2 c])

connected :: String -> [Conn] -> S.Set String
connected cs = allComps . filter (\c -> cc1 c == cs || cc2 c == cs)

allConnections :: [Conn] -> M.Map String (S.Set String)
allConnections conns =
  M.fromList . fmap (\cs -> (cs, connected cs conns)) . S.toList . allComps
    $ conns

findNetworks :: Int -> M.Map String (S.Set String) -> S.Set (S.Set String)
findNetworks n nm =
  S.fromList
    . fmap S.fromList
    . concatMap (\cs -> doer (n - 1) [] (S.filter (/= cs) allCompsSet) cs)
    . M.keys
    $ nm
  where
    allCompsSet = M.keysSet nm
    doer :: Int -> [String] -> S.Set String -> String -> [[String]]
    doer n' netw possible cs
      | n' == 0 = [cs : netw]
      | otherwise =
        case cs `M.lookup` nm of
          Just cnts
            | S.size is >= n' ->
              concatMap
                (\cs' -> doer (n' - 1) (cs : netw) (S.filter (/= cs') is) cs')
                . S.toList
                $ is
            | otherwise -> []
            where is = S.intersection possible cnts
          Nothing -> []

part1Solution :: [String] -> Int
part1Solution =
  S.size
    . S.filter (any ((== 't') . head))
    . findNetworks 3
    . allConnections
    . fmap read

findLargestNetworks :: M.Map String (S.Set String) -> S.Set (S.Set String)
findLargestNetworks nm =
  S.fromList
    . fmap S.fromList
    . fst
    . foldl'
        (\(netws, nm') cs ->
           let (netws', nm'') = doer nm' [] (S.filter (/= cs) allCompsSet) cs
            in (netws' <> netws, nm''))
        ([], nm)
    . M.keys
    $ nm
  where
    allCompsSet = M.keysSet nm
    doer ::
         M.Map String (S.Set String)
      -> [String]
      -> S.Set String
      -> String
      -> ([[String]], M.Map String (S.Set String))
    doer nm' netw possible cs =
      case cs `M.lookup` nm' of
        Just cnts
          | S.size is > 0 ->
            foldl'
              (\(netws'', nm'') cs' ->
                 let (netws''', nm''') =
                       doer
                         (M.delete cs nm'')
                         (cs : netw)
                         (S.filter (/= cs') is)
                         cs'
                  in (netws''' <> netws'', nm'''))
              ([], nm')
              . S.toList
              $ is
          | otherwise -> ([cs : netw], M.delete cs nm')
          where is = S.intersection possible cnts
        Nothing -> ([], nm')

part2Solution :: [String] -> String
part2Solution css =
  intercalate "," . S.toList . head . filter ((== maxSize) . S.size) . S.toList
    $ largests
  where
    largests = findLargestNetworks . allConnections . fmap read $ css
    maxSize = maximum . fmap S.size . S.toList $ largests
