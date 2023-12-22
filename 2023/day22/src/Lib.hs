module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', sortOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Point = P {px :: Int, py :: Int, pz :: Int} deriving (Show, Eq, Ord)

instance Read Point where
  readPrec = do
    x <- readPrec
    _ <- lift . P.char $ ','
    y <- readPrec
    _ <- lift . P.char $ ','
    P x y <$> readPrec

data Block = Block {bstart :: Point, bend :: Point} deriving (Show, Eq, Ord)

instance Read Block where
  readPrec = do
    p1 <- readPrec
    _ <- lift . P.char $ '~'
    Block p1 <$> readPrec

haveCommonCoordinate :: Int -> Int -> Int -> Int -> Bool
haveCommonCoordinate x11 x12 x21 x22 = not $ x11 < min x21 x22 && x12 < min x21 x22 || x11 > max x21 x22 && x12 > max x21 x22

haveCommonPoint :: Block -> Block -> Bool
haveCommonPoint (Block (P x11 y11 z11) (P x12 y12 z12)) (Block (P x21 y21 z21) (P x22 y22 z22)) =
  haveCommonCoordinate x11 x12 x21 x22
    && haveCommonCoordinate y11 y12 y21 y22
    && haveCommonCoordinate z11 z12 z21 z22

bringBlocksDown :: [Block] -> [Block]
bringBlocksDown bs =
  foldl'
    ( \bs'' b ->
        let z0 =
              maximum
                . (1 :)
                . fmap (\(z, _) -> z + 1)
                . filter (uncurry haveCommonPoint . snd)
                . fmap (\b1 -> let zm = max (pz . bstart $ b1) (pz . bend $ b1) in (zm, (b1, setMinZto b zm)))
                $ bs''
         in (setMinZto b z0 : bs'')
    )
    []
    bs'
  where
    bs' = sortOn (\b -> min (pz . bstart $ b) (pz . bend $ b)) bs

setMinZto :: Block -> Int -> Block
setMinZto b z
  | (pz . bstart) b == (pz . bend) b = b {bstart = (bstart b) {pz = z}, bend = (bend b) {pz = z}}
  | (pz . bstart) b < (pz . bend) b = b {bstart = (bstart b) {pz = z}, bend = (bend b) {pz = z + (pz . bend) b - (pz . bstart) b}}
  | otherwise = b {bend = (bend b) {pz = z}, bstart = (bstart b) {pz = z + (pz . bstart) b - (pz . bend) b}}

mkSupportGraph :: [Block] -> M.Map Block [Block]
mkSupportGraph bs =
  M.fromList
    . fmap
      ( \b ->
          ( b,
            filter
              ( \b' ->
                  max (pz . bstart $ b) (pz . bend $ b) == min (pz . bstart $ b') (pz . bend $ b') - 1
                    && haveCommonPoint b (b' {bstart = (bstart b') {pz = (pz . bstart) b' - 1}, bend = (bend b') {pz = (pz . bend) b' - 1}})
              )
              bs
          )
      )
    $ bs

mkSupportedByGraph :: [Block] -> M.Map Block [Block]
mkSupportedByGraph bs =
  M.fromList
    . fmap
      ( \b ->
          ( b,
            filter
              ( \b' ->
                  max (pz . bstart $ b') (pz . bend $ b') == min (pz . bstart $ b) (pz . bend $ b) - 1
                    && haveCommonPoint b' (b {bstart = (bstart b) {pz = (pz . bstart) b - 1}, bend = (bend b) {pz = (pz . bend) b - 1}})
              )
              bs
          )
      )
    $ bs

part1Solution :: [String] -> Int
part1Solution css =
  length
    . foldl'
      ( \eliminated b ->
          let supports = fromMaybe [] . (`M.lookup` supportGraph) $ b
              theyAllSupported =
                all (any (/= b) . fromMaybe [] . (`M.lookup` supportedByGraph)) supports
           in if null supports || theyAllSupported
                then b : eliminated
                else eliminated
      )
      []
    $ bs
  where
    bs = bringBlocksDown . fmap read $ css
    supportGraph = mkSupportGraph bs
    supportedByGraph = mkSupportedByGraph bs

chainReaction :: M.Map Block [Block] -> M.Map Block [Block] -> [Block] -> Block -> (Int, [Block])
chainReaction supportGraph supportedByGraph eliminated b =
  foldl'
    ( \(x, eliminated') b' ->
        let (x', eliminated'') = chainReaction supportGraph supportedByGraph (b' : eliminated') b'
         in (x + x', eliminated'')
    )
    (length theySupportedBy, eliminated)
    theySupportedBy
  where
    supports = fromMaybe [] . (`M.lookup` supportGraph) $ b
    theySupportedBy =
      fmap fst
        . filter (null . snd)
        . fmap
          ( \b' ->
              let alsoSupport = filter (\b'' -> b'' /= b && b'' `notElem` eliminated) . fromMaybe [] . (`M.lookup` supportedByGraph) $ b'
               in (b', alsoSupport)
          )
        $ supports

part2Solution :: [String] -> Int
part2Solution css =
  sum
    . fmap (fst . chainReaction supportGraph supportedByGraph [])
    $ bs
  where
    bs = bringBlocksDown . fmap read $ css
    supportGraph = mkSupportGraph bs
    supportedByGraph = mkSupportedByGraph bs
