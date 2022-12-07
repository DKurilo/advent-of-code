module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl')
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data FS = F String Int | D String (Maybe Int) deriving (Show)

parseRestOfLine :: PC.ReadPrec String
parseRestOfLine = lift . P.munch1 $ (/= '\n')

instance Read FS where
  readPrec =
    ( do
        _ <- lift . P.string $ "dir"
        lift P.skipSpaces
        (`D` Nothing) <$> parseRestOfLine
    )
      PC.<++ ( do
                 size <- readPrec
                 lift P.skipSpaces
                 (`F` size) <$> parseRestOfLine
             )

data Command = CDUP | CDTOP | CD String | LS [FS] deriving (Show)

instance Read Command where
  readPrec = do
    _ <- lift . P.char $ '$'
    lift P.skipSpaces
    ( do
        _ <- lift . P.string $ "cd .."
        return CDUP
      )
      PC.<++ ( do
                 _ <- lift . P.string $ "cd /"
                 return CDTOP
             )
      PC.<++ ( do
                 _ <- lift . P.string $ "cd"
                 lift P.skipSpaces
                 CD <$> parseRestOfLine
             )
      PC.<++ ( do
                 _ <- lift . P.string $ "ls"
                 lift P.skipSpaces
                 output <-
                   lift . P.many $
                     ( do
                         fs <- PC.readPrec_to_P readPrec 0
                         P.skipSpaces
                         return fs
                     )
                 return . LS $ output
             )

newtype Log = Log {unCmds :: [Command]} deriving (Show)

instance Read Log where
  readPrec =
    Log
      <$> ( lift . P.many1 $
              ( do
                  cmd <- PC.readPrec_to_P readPrec 0
                  P.skipSpaces
                  return cmd
              )
          )

data FileTree a
  = Dir a String [FileTree a]
  | File Int String
  deriving (Show)

isDir :: FileTree a -> Bool
isDir Dir {} = True
isDir _ = False

ftName :: FileTree a -> String
ftName (Dir _ name _) = name
ftName (File _ name) = name

ftSub :: FileTree a -> [FileTree a]
ftSub (Dir _ _ sub) = sub
ftSub File {} = []

ftSize :: (Int -> a) -> FileTree a -> a
ftSize _ (Dir size _ _) = size
ftSize f (File size _) = f size

insertFS :: [FS] -> [String] -> FileTree (Maybe Int) -> FileTree (Maybe Int)
insertFS _ _ ft@(File _ _) = ft
insertFS xs [] (Dir size name sub) = Dir size name (foldl' doer sub xs)
  where
    doer :: [FileTree (Maybe Int)] -> FS -> [FileTree (Maybe Int)]
    doer fts (F name' size') = File size' name' : fts
    doer fts (D name' size') = Dir size' name' [] : fts
insertFS xs (d : path) (Dir size name sub) = Dir size name (map doer sub)
  where
    doer ft'@(File _ _) = ft'
    doer ft'
      | ftName ft' == d = insertFS xs path ft'
      | otherwise = ft'

mkInitialFileTree :: Log -> FileTree (Maybe Int)
mkInitialFileTree = snd . foldl' doer ([], Dir Nothing "/" []) . unCmds
  where
    doer :: ([String], FileTree (Maybe Int)) -> Command -> ([String], FileTree (Maybe Int))
    doer (path, fs) CDUP = (tail path, fs)
    doer (_, fs) CDTOP = ([], fs)
    doer (path, fs) (CD name) = (name : path, fs)
    doer (path, fs) (LS ls) = (path, insertFS ls (reverse path) fs)

calcSizes :: FileTree (Maybe Int) -> FileTree Int
calcSizes (File name size) = File name size
calcSizes (Dir size name sub) = case size of
  Just s -> Dir s name sub'
  _ -> Dir (sum . map (ftSize id) $ sub') name sub'
  where
    sub' = map calcSizes sub

flatFilterFT :: (FileTree a -> Bool) -> FileTree a -> [FileTree a]
flatFilterFT p ft
  | p ft = ft : sub
  | isDir ft = sub
  | otherwise = []
  where
    sub = concatMap (flatFilterFT p) . ftSub $ ft

part1Solution :: Log -> Int
part1Solution = sum . map (ftSize id) . flatFilterFT (\ft -> isDir ft && ftSize id ft <= 100000) . calcSizes . mkInitialFileTree

part2Solution :: Log -> Int
part2Solution l = minimum . map (ftSize id) . flatFilterFT (\ft' -> isDir ft' && ftSize id ft' >= toRemove) $ ft
  where
    ft = calcSizes . mkInitialFileTree $ l
    toRemove = 30000000 - 70000000 + ftSize id ft
