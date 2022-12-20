module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.List (foldl', maximumBy)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Robot = Ore | Clay | Obsidian | Geode deriving (Eq, Show)

instance Read Robot where
  readPrec =
    lift $
      (P.string "ore" >> return Ore)
        P.<++ (P.string "clay" >> return Clay)
        P.<++ (P.string "obsidian" >> return Obsidian)
        P.<++ (P.string "geode" >> return Geode)

data Recipe = Recipe {rRobot :: Robot, rOre :: Int, rClay :: Int, rObsidian :: Int} deriving (Show)

instance Read Recipe where
  readPrec = do
    _ <- lift . P.string $ "Each"
    lift P.skipSpaces
    rt <- readPrec
    lift P.skipSpaces
    _ <- lift . P.string $ "robot costs"
    ingredients <- lift . P.many1 $ do
      P.skipSpaces
      P.optional . P.string $ "and"
      P.skipSpaces
      n <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      ing <- PC.readPrec_to_P readPrec 0
      return (ing, n)
    _ <- lift . P.char $ '.'
    return
      . foldl'
        ( \r i -> case fst i of
            Ore -> r {rOre = snd i}
            Clay -> r {rClay = snd i}
            Obsidian -> r {rObsidian = snd i}
            _ -> r
        )
        (Recipe rt 0 0 0)
      $ ingredients

data Blueprint = Blueprint {bId :: Int, bRecipes :: [Recipe]} deriving (Show)

instance Read Blueprint where
  readPrec = do
    _ <- lift . P.string $ "Blueprint"
    lift P.skipSpaces
    n <- readPrec
    _ <- lift . P.char $ ':'
    lift P.skipSpaces
    rs <- lift . P.many1 $ do
      r <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      return r
    return $ Blueprint n rs

data Task = Task {tOre :: Int, tClay :: Int, tObsidian :: Int, tGeode :: Int} deriving (Show)

data State = State
  { sOre :: Int,
    sClay :: Int,
    sObsidian :: Int,
    sGeode :: Int,
    sRobotOre :: Int,
    sRobotClay :: Int,
    sRobotObsidian :: Int,
    sRobotGeode :: Int
  }
  deriving (Eq, Ord, Show)

canBuild :: Recipe -> State -> Bool
canBuild r s = rOre r <= sOre s && rClay r <= sClay s && rObsidian r <= sObsidian s

build :: Recipe -> State -> State
build r s = case rRobot r of
  Ore -> s' {sRobotOre = sRobotOre s + 1}
  Clay -> s' {sRobotClay = sRobotClay s + 1}
  Obsidian -> s' {sRobotObsidian = sRobotObsidian s + 1}
  Geode -> s' {sRobotGeode = sRobotGeode s + 1}
  where
    s' = s {sOre = sOre s - rOre r, sClay = sClay s - rClay r, sObsidian = sObsidian s - rObsidian r}

incS :: State -> State
incS s =
  s
    { sOre = sOre s + sRobotOre s,
      sClay = sClay s + sRobotClay s,
      sObsidian = sObsidian s + sRobotObsidian s,
      sGeode = sGeode s + sRobotGeode s
    }

findMaxGeode :: Blueprint -> Int -> State
findMaxGeode b = maximumBy (\s1 s2 -> sGeode s1 `compare` sGeode s2) . doer [s0] S.empty
  where
    s0 = State 0 0 0 0 1 0 0 0
    maxOre = maximum . map rOre . bRecipes $ b
    findRecipe :: Robot -> Recipe
    findRecipe r = head . filter ((== r) . rRobot) . bRecipes $ b
    oreRecipe = findRecipe Ore
    clayRecipe = findRecipe Clay
    obsidianRecipe = findRecipe Obsidian
    geodeRecipe = findRecipe Geode
    doer :: [State] -> S.Set State -> Int -> [State]
    doer front visited time
      | time == 0 = front
      | otherwise = doer front' visited' (time - 1)
      where
        (front', visited') = foldl' nextState ([], visited) front
        nextState :: ([State], S.Set State) -> State -> ([State], S.Set State)
        nextState (front'', visited'') s
          | s `S.member` visited'' = (front'', visited'')
          | otherwise = (newFront <> front'', S.insert s visited'')
          where
            s' = incS s
            -- map here is the most weird part
            -- it allows to move all cases where we have more resources of some time
            -- than we ever need to the same state
            -- without this map it will work VERY long time
            newFront =
              map
                ( \s'' ->
                    s''
                      { sOre = min (sOre s'') (time * maxOre - sRobotOre s'' * (time - 2)),
                        sClay = min (sClay s'') (time * rClay obsidianRecipe - sRobotClay s'' * (time - 2)),
                        sObsidian = min (sObsidian s'') (time * rObsidian geodeRecipe - sRobotObsidian s'' * (time - 2))
                      }
                )
                $ [s']
                  <> ( if canBuild geodeRecipe s
                         then [build geodeRecipe s']
                         else
                           if canBuild obsidianRecipe s
                             then [build obsidianRecipe s']
                             else [build clayRecipe s' | canBuild clayRecipe s]
                     )
                  <> [build oreRecipe s' | canBuild oreRecipe s && sRobotOre s <= maxOre]

part1Solution :: [Blueprint] -> Int
part1Solution = sum . map (\b -> bId b * (sGeode . findMaxGeode b) 24)

part2Solution :: [Blueprint] -> Int
part2Solution = product . map (\b -> let s = findMaxGeode b 32 in trace (show (bId b, s)) $ sGeode s) . take 3
