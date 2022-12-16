module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', nub)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Valve = Valve {vName :: String, vFlow :: Int, vValves :: [String], vOpened :: Bool, vTime :: Int} deriving (Show)

instance Read Valve where
  readPrec = do
    _ <- lift . P.string $ "Valve"
    lift P.skipSpaces
    name <- lift . P.munch1 $ (/= ' ')
    lift P.skipSpaces
    _ <- lift . P.string $ "has flow rate="
    flow <- readPrec
    _ <- lift . P.string $ "; tunnel"
    _ <- lift . P.optional . P.char $ 's'
    lift P.skipSpaces
    _ <- lift . P.string $ "lead"
    _ <- lift . P.optional . P.char $ 's'
    lift P.skipSpaces
    _ <- (lift . P.string $ "to valves") PC.<++ (lift . P.string $ "to valve")
    lift P.skipSpaces
    valve1 <- lift . P.munch1 $ (\c -> c /= ' ' && c /= ',' && c /= '\n')
    valves <- lift . P.many $ do
      _ <- P.char ','
      P.skipSpaces
      P.munch1 (\c -> c /= ' ' && c /= ',' && c /= '\n')
    return $ Valve name flow (valve1 : valves) False 0

newtype Cave = Cave {unCave :: M.Map String Valve} deriving (Show)

instance Read Cave where
  readPrec = do
    valves <- lift . P.many1 $ do
      valve <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      return valve
    return . Cave . M.fromList . map (\v -> (vName v, v)) $ valves

findPath :: String -> String -> Cave -> Int
findPath from to cave = doer (S.singleton from) (S.singleton from) 0
  where
    doer :: S.Set String -> S.Set String -> Int -> Int
    doer front visited currentStep
      | to `S.member` front = currentStep
      | null front = error "not accessible valve"
      | otherwise = doer front' (S.union visited front') (currentStep + 1)
      where
        front' =
          S.fromList
            . filter (not . (`S.member` visited))
            . concatMap (\name -> maybe [] vValves (M.lookup name . unCave $ cave))
            . S.toList
            $ front

data Weights = Weights {wTime :: Int, wFlow :: Int} deriving (Show)

mkWeights :: Cave -> String -> String -> Weights
mkWeights cave from to = Weights (findPath from to cave) (maybe 0 vFlow . M.lookup to . unCave $ cave)

findPathes :: Cave -> M.Map (String, String) Weights
findPathes cave = M.fromList [((from, to), mkWeights cave from to) | from <- valves, to <- valves]
  where
    valves = M.keys . unCave $ cave

findAllPathes :: Int -> Cave -> M.Map (Int, [String]) (Int, [String])
findAllPathes maxTime cave = doer "AA" 0 0 [] M.empty
  where
    pathes = findPathes cave
    doer :: String -> Int -> Int -> [String] -> M.Map (Int, [String]) (Int, [String]) -> M.Map (Int, [String]) (Int, [String])
    doer from released time opened steps
      | time == maxTime = steps
      | null ws =
        doer
          from
          released
          (time + 1)
          opened
          ( case (time + 1, opened) `M.lookup` steps of
              Just (released', _)
                | released' > released -> steps
              _ -> M.insert (time + 1, opened) (released, reverse opened) steps
          )
      | otherwise =
        foldl'
          ( \steps' (from', time', released') ->
              case (time', from' : opened) `M.lookup` steps' of
                Just (released'', _)
                  | released'' > released' -> steps'
                _ -> doer from' released' time' (from' : opened) (M.insert (time', from' : opened) (released', reverse (from' : opened)) steps')
          )
          steps
          ws
      where
        ws =
          [ (to, time', flow + released)
            | to <- M.keys . unCave $ cave,
              (not . (`elem` opened)) to,
              let weights = fromMaybe (Weights 100000 0) . M.lookup (from, to) $ pathes,
              wFlow weights /= 0,
              let time' = time + wTime weights + 1,
              time' <= maxTime,
              let flow = (maxTime - time') * wFlow weights
          ]

findReleased :: Cave -> Int
findReleased = fromMaybe 0 . M.lookup 30 . M.mapKeysWith max fst . M.map fst . findAllPathes 30

part1Solution :: Cave -> Int
part1Solution = findReleased

findReleasedWithElephant :: Cave -> Int
findReleasedWithElephant cave =
  fst
    . maximum
    $ [ (fst myPath + fst elephantPath, (myPath, elephantPath))
        | myPath <- paths,
          elephantPath <- paths,
          not . any (`elem` snd elephantPath) . snd $ myPath
      ]
  where
    paths = nub . M.elems . findAllPathes 26 $ cave

part2Solution :: Cave -> Int
part2Solution = findReleasedWithElephant
