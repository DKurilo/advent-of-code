{-# LANGUAGE TupleSections #-}
module Lib
    ( part1solution
    , part2solution
    ) where

import           Control.Applicative          ((<|>))
import           Data.Bifunctor               (second)
import           Data.List                    (intercalate, nub, sortOn)
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.Vector                  as V
import           GHC.Read
import qualified Text.ParserCombinators.ReadP as P
import           Text.Read

data Tile = Tile Int  (V.Vector (V.Vector Char)) deriving (Eq, Show)

instance Read Tile where
    readPrec = do
        lift $ P.string "Tile"
        lift P.skipSpaces
        n <- readPrec
        lift $ P.char ':'
        lift P.skipSpaces
        Tile n . V.fromList <$> lift (P.many1 (do
            cs <- P.munch1 (\c -> c == '.' || c == '#')
            P.char '\n'
            (return . V.fromList) cs))

newtype Tiles = Tiles [Tile] deriving (Eq, Show)

pTiles :: ReadPrec [Tile]
pTiles =  do
    t <- readPrec
    lift P.skipSpaces
    ts <- pTiles <|> return []
    return (t:ts)

instance Read Tiles where
    readPrec = Tiles <$> pTiles

tRotate :: Tile -> Tile
tRotate (Tile n css) = (Tile n . V.fromList) [V.fromList [css V.! (len - i) V.! j | i <- [0..len]] | j <- [0..len]]
    where len = length css - 1

tFlip :: Tile -> Tile
tFlip (Tile n css) = Tile n . V.map V.reverse $ css

allMods :: [Tile] -> Tile -> [Tile]
allMods ts (Tile n _) = (M.fromList . map (\t'@(Tile n' _) -> (n', rots t' ++ (rots . tFlip) t'))) ts M.! n
    where rots t' = scanr (const tRotate) t' [0..2]

tryToPlace :: Tile -> (Int, Int) -> M.Map (Int, Int) Tile -> Bool
tryToPlace (Tile _ css) p@(i, j) placed = checkTop && checkRight && checkBottom && checkLeft
    where checkTop = case (i - 1, j) `M.lookup` placed of
                       Just (Tile _ css') -> V.last css' == V.head css
                       _                  -> True
          checkBottom = case (i + 1, j) `M.lookup` placed of
                          Just (Tile _ css') -> V.head css' == V.last css
                          _                  -> True
          checkLeft = case (i, j - 1) `M.lookup` placed of
                        Just (Tile _ css') -> V.all (\(r, l) -> V.last r == V.head l) . V.zip css' $ css
                        _ -> True
          checkRight = case (i, j + 1) `M.lookup` placed of
                         Just (Tile _ css') -> V.all (\(r, l) -> V.last r == V.head l) . V.zip css $ css'
                         _ -> True

findAllPossible :: (Tile -> [Tile]) -> Int -> [Tile] -> M.Map (Int, Int) Tile -> Maybe (M.Map (Int, Int) Tile)
findAllPossible _ _ [] placed = Just placed
findAllPossible allMods' gridSize ts placed
  | maxI - minI >= gridSize || maxJ - minJ >= gridSize = Nothing
  | (not . null) shouldFill && (not . null) found = head found
  | (not . null) shouldFill = Nothing
  | (not . null) inner && (not . null) found' = head found'
  | (not . null) inner = Nothing
  | (not . null) found'' = head found''
  | otherwise = Nothing
  where minI = (minimum . map fst . M.keys) placed
        minJ = (minimum . map snd . M.keys) placed
        maxI = (maximum . map fst . M.keys) placed
        maxJ = (maximum . map snd . M.keys) placed
        places i j = [(i - 1, j), (i, j + 1) , (i + 1, j), (i, j - 1)]
        front = nub . concatMap (\(i, j) -> filter (`M.notMember` placed) (places i j)) . M.keys $ placed
        shouldFill = filter (\(i, j) -> (length . filter (`M.member` placed)) (places i j) > 1) front
        foundFor p = filter (/=Nothing)
                   . map (\t@(Tile n _) -> findAllPossible allMods' gridSize (filter (\(Tile n' _) -> n' /= n) ts) (M.insert p t placed))
                   . filter (\t -> tryToPlace t p placed) . concatMap allMods' $ ts
        found = (foundFor . head) shouldFill
        inner = filter (\(i, j) -> i >= minI && i <= maxI && j >= minJ && j <= maxJ) front
        found' = (foundFor . head) inner
        bottom = filter (\(i, _) -> i > maxI) front
        top = filter (\(i, _) -> i < minI) front
        left = filter (\(_, j) -> j < minJ) front
        right = filter (\(_, j) -> j > maxJ) front
        ordered = sortOn ((*(-1)) . length) [top, left, right, bottom]
        found'' = concatMap (\l -> if (not . null) l then (foundFor . head) l else []) ordered

orderTiles :: Tiles -> Maybe (M.Map (Int, Int) Tile, Int)
orderTiles (Tiles (t:ts)) = (, gridSize)
                           <$> findAllPossible (allMods ts) gridSize ts (M.singleton (0,0) t)
    where gridSize = round . sqrt . fromIntegral . length $ ts

getPixels :: Tile -> V.Vector (V.Vector Char)
getPixels (Tile _ ps) = ps

prepareImage :: M.Map (Int, Int) Tile -> Tile
prepareImage ts = Tile 0 . V.fromList . concat $ [ concatTiles [(V.map (V.init . V.tail) . V.init . V.tail) (getPixels (ts M.! (i, j)))
                                                               | j <- [minJ..maxJ]
                                                               ] | i <- [minI..maxI]
                                                 ]
    where (minI, minJ) = (minimum . M.keys) ts
          (maxI, maxJ) = (maximum . M.keys) ts
          concatTiles :: [V.Vector (V.Vector Char)] -> [V.Vector Char]
          concatTiles vs = [V.concat [vs !! j V.! i | j <- [0..(length vs - 1)]] | i <- [0..((V.length .head) vs - 1)]]

input :: IO Tiles
input = read <$> readFile "input"

getCornerProduct :: (M.Map (Int, Int) Tile, Int) -> Int
getCornerProduct (ts, gridSize) = tl * tr * br * bl
    where (tli, tlj) = (minimum . M.keys) ts
          tl = case (tli, tlj) `M.lookup` ts of
                 Just (Tile n _) -> n
                 _               -> 0
          tr = case (tli, tlj + gridSize - 1) `M.lookup` ts of
                 Just (Tile n _) -> n
                 _               -> 0
          br = case (tli + gridSize - 1, tlj + gridSize - 1) `M.lookup` ts of
                 Just (Tile n _) -> n
                 _               -> 0
          bl = case (tli + gridSize - 1, tlj) `M.lookup` ts of
                 Just (Tile n _) -> n
                 _               -> 0


part1solution :: IO ()
part1solution = print . fmap getCornerProduct . orderTiles =<< input

findSeaMonster :: Tile -> S.Set (Int, Int)
findSeaMonster t = S.fromList . concatMap (\(r, f, t') -> (concatMap (unflip f . unrot r) . seaMonsters) t') $ imgs
    where imgSize = (V.length . getPixels) t - 1
          imgs = let ft = tFlip t in zipWith (\r t' -> (r, False, t')) [3, 2..0] (scanr (const tRotate) t [0..2])
                                     ++ zipWith (\r t' -> (r, True, t')) [3, 2..0] (scanr (const tRotate) ft [0..2])
          unrot :: Int -> [(Int, Int)] -> [(Int, Int)]
          unrot 0 ps = ps
          unrot n ps = unrot (n - 1) . map (\(i, j) -> (imgSize - j, i)) $ ps
          unflip False ps = ps
          unflip True ps  = map (second (imgSize -)) ps
          seaMonsters :: Tile -> [[(Int, Int)]]
          seaMonsters (Tile _ ps) = foldl (\sms (i,j) ->
              if fl (V.slice j 20 (ps V.! i)) && sl (V.slice j 20 (ps V.! (i + 1))) && tl (V.slice j 20 (ps V.! (i + 2)))
                 then sm i j : sms
                 else sms) [] [(i, j) | i <- [0..V.length ps - 3], j <- [0..(V.length (ps V.! 0) - 21)]]
          cl l = V.all (\(m, s) -> m == ' ' || m == '#' && s == '#') . V.zip (V.fromList l)
          fl = cl "                  # "
          sl = cl "#    ##    ##    ###"
          tl = cl " #  #  #  #  #  #   "
          sm i j = [ (i, j + 18)
                   , (i + 1, j), (i + 1, j + 5), (i + 1, j + 6), (i + 1, j + 11), (i + 1, j + 12)
                   , (i + 1, j + 17), (i + 1, j + 18), (i + 1, j + 19)
                   , (i + 2, j + 1), (i + 2, j + 4), (i + 2, j + 7), (i + 2, j + 10), (i + 2, j + 13), (i + 2, j + 16)
                   ]

setPointsTo :: Char -> S.Set (Int, Int) -> Tile -> Tile
setPointsTo c s (Tile n ps) = (Tile n . foldl (\ps' (i, j) -> ps' V.// [(i, ps' V.! i V.// [(j, c)])]) ps . S.toList) s

countPoints :: Char -> Tile -> Int
countPoints c = V.sum . V.map (V.length . V.filter (==c)) . getPixels

showImg :: Tile -> String
showImg (Tile _ ts) = intercalate "\n" . V.toList . V.map V.toList $ ts

part2solution :: IO ()
part2solution = do
    (Just t) <- fmap (prepareImage . fst) . orderTiles <$> input
    let sm = findSeaMonster t
    -- putStrLn . showImg $ t
    -- putStrLn "\n"
    -- putStrLn . showImg . setPointsTo 'O' sm $ t
    print . countPoints '#' . setPointsTo 'O' sm $ t
