module Lib (
    part1Solution,
    part2Solution,
)
where

import Data.Char (isDigit)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Shape = Shape {sIndex :: Int, sPart :: S.Set (Int, Int)} deriving (Eq, Show)

instance Read Shape where
    readPrec = lift $ do
        i <- fmap read . P.munch1 $ isDigit
        _ <- P.char ':'
        P.skipSpaces
        bs <- P.many1 $ do
            cs <- P.munch1 (\c -> c == '#' || c == '.')
            P.skipSpaces
            return cs
        return
            . Shape i
            . S.fromList
            . concatMap (fmap snd . filter ((== '#') . fst))
            . zipWith (\j -> zipWith (\i c -> (c, (i, j))) [0 ..]) [0 ..]
            $ bs

data Region = Region
    { rW :: Int
    , rH :: Int
    , rGifts :: M.Map Int Int
    , rSet :: S.Set (Int, Int)
    , rTried :: S.Set ((Int, Int), Int, Int)
    }
    deriving (Eq, Show)

instance Read Region where
    readPrec = lift $ do
        w <- fmap read . P.munch1 $ isDigit
        _ <- P.char 'x'
        h <- fmap read . P.munch1 $ isDigit
        _ <- P.char ':'
        gs <- P.many1 $ do
            P.skipSpaces
            fmap read . P.munch1 $ isDigit
        P.skipSpaces
        return . Region w h (M.fromList . zip [0 ..] $ gs) S.empty $ S.empty

data Input = Input {iShapes :: [Shape], iReg :: [Region]} deriving (Eq, Show)

instance Read Input where
    readPrec = lift $ do
        shs <- P.many1 $ do
            sh <- PC.readPrec_to_P readPrec 0
            P.skipSpaces
            return sh
        rs <- P.many1 $ do
            r <- PC.readPrec_to_P readPrec 0
            P.skipSpaces
            return r
        P.skipSpaces
        return . Input shs $ rs

rotCWShape :: Shape -> Shape
rotCWShape s = s{sPart = S.map (\(x, y) -> (y, -x)) . sPart $ s}

flipYShape :: Shape -> Shape
flipYShape s = s{sPart = S.map (\(x, y) -> (-y, x)) . sPart $ s}

allTransforms =
    [ id
    , rotCWShape
    , rotCWShape . rotCWShape
    , rotCWShape . rotCWShape . rotCWShape
    , flipYShape
    , flipYShape . rotCWShape
    , flipYShape . rotCWShape . rotCWShape
    , flipYShape . rotCWShape . rotCWShape . rotCWShape
    ]

allTransformsCount = length allTransforms

isFit :: Shape -> (Int, Int) -> Region -> Bool
isFit s (x, y) r =
    all
        ( \(x', y') ->
            let x'' = x' + dx
                y'' = y' + dy
             in x'' >= 0 && y'' >= 0 && x'' < rW r && y'' < rH r && (x'', y'') `S.notMember` rSet r
        )
        . S.toList
        . sPart
        $ s
  where
    (x0, y0) = S.findMin . sPart $ s
    dx = x - x0
    dy = y - y0

placeShape :: Shape -> Region -> Maybe (Region, (Int, Int), Int)
placeShape s r =
    fmap
        ( \((x, y), tr) ->
            let s' = (allTransforms !! tr) s
                (x0, y0) = S.findMin . sPart $ s'
                dx = x - x0
                dy = y - y0
                r' =
                    r
                        { rSet = rSet r <> (S.map (\(x', y') -> (x' + dx, y' + dy)) . sPart) s'
                        , rTried = S.insert ((x, y), tr, sIndex s) . rTried $ r
                        , rGifts = (M.insertWith (+) (sIndex s) (-1) . rGifts) r
                        }
             in (r', (x, y), tr)
        )
        findPointTransform
  where
    findPointTransform :: Maybe ((Int, Int), Int)
    findPointTransform
        | null allowed = Nothing
        | otherwise = Just . head $ allowed
      where
        allowed =
            [ ((x, y), tr)
            | x <- [0 .. rW r - 1]
            , y <- [0 .. rH r - 1]
            , tr <- [0 .. allTransformsCount - 1]
            , ((x, y), tr, sIndex s) `S.notMember` rTried r
            , let s' = (allTransforms !! tr) s
            , isFit s' (x, y) r
            ]

placeAllShapes :: [Shape] -> Region -> Maybe Region
placeAllShapes [] r = Just r
placeAllShapes (s : shs) r = case placeShape s r of
    Just (r', (x, y), tr) -> case placeAllShapes shs r' of
        Just r'' -> Just r''
        Nothing -> placeAllShapes (s : shs) (r{rTried = rTried r'})
    Nothing -> Nothing

part1Solution :: Input -> Int
part1Solution i = length . filter (\r -> rW r `div` 4 * rH r `div` 4 * 2 >= (sum . M.elems . rGifts) r ) . iReg $ i
--     length
--         . (\x -> trace (show x) x)
--         . mapMaybe (\r -> placeAllShapes (concatMap (\(si, rep) -> replicate rep (iShapes i !! si)) . M.toList . rGifts $ r) r)
--         . iReg
--         $ i

part2Solution :: Input -> Int
part2Solution = length . iReg
