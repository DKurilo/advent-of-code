module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Function (on)
import Data.List (elemIndex, minimumBy, nub, sort)
import Data.Maybe (fromMaybe)
import Data.Ord (Ord (..), compare)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

newtype Step = Step {unStep :: Char} deriving (Eq, Ord, Show)

instance Read Step where
  readPrec = Step <$> lift P.get

data Rule = Rule {rStep :: Step, rBefore :: Step} deriving (Eq, Ord, Show)

instance Read Rule where
  readPrec = do
    lift . P.string $ "Step "
    st <- readPrec
    lift . P.string $ " must be finished before step "
    stB <- readPrec
    lift . P.string $ " can begin."
    return $ Rule st stB

allSteps :: [Rule] -> [Step]
allSteps = sort . nub . concatMap (\r -> [rStep r, rBefore r])

isBlocked :: Step -> [Rule] -> Bool
isBlocked st = any ((== st) . rBefore)

stepsSequence :: [Rule] -> [Step]
stepsSequence rs = doer [] [] (allSteps rs) rs
  where
    doer :: [Step] -> [Step] -> [Step] -> [Rule] -> [Step]
    doer done notnow todo [] = reverse $ todo ++ done
    doer done notnow [] rs' = error "I cannot finish it"
    doer done notnow (st : todo) rs'
      | isBlocked st rs' = doer done (st : notnow) todo rs'
      | otherwise = doer (st : done) [] (reverse notnow ++ todo) (filter ((/= st) . rStep) rs')

part1Solution :: [Rule] -> String
part1Solution = map unStep . stepsSequence

timeToDo :: Int -> Step -> Int
timeToDo baseTime st = baseTime + 1 + fromMaybe 0 (unStep st `elemIndex` ['A' .. 'Z'])

data Job = Job {jbStep :: Step, jbTimeLeft :: Int} deriving (Eq, Show)

instance Ord Job where
  a <= b = jbStep a <= jbStep b

data Pool = Pool {plSize :: Int, plJobs :: [Job]} deriving (Eq, Show)

assembleSleigh :: [Rule] -> Pool -> Int -> Int
assembleSleigh baseRs initPool baseTime = doer initPool allJobs baseRs
  where
    allJobs = sort . map (\st -> Job st (timeToDo baseTime st)) . allSteps $ baseRs
    doer :: Pool -> [Job] -> [Rule] -> Int
    doer p [] rs = maximum . map jbTimeLeft . plJobs $ p
    doer p todo rs
      | (null . plJobs) p = doer pStart todoStart rs
      | otherwise = t + doer p' todo' rs'
      where
        canDoNowStart = filter (not . (`isBlocked` rs) . jbStep) todo
        willDoNowStart = take (plSize p) canDoNowStart
        pStart = p {plJobs = willDoNowStart}
        todoStart = filter (`notElem` willDoNowStart) todo
        done = minimumBy (compare `on` jbTimeLeft) . plJobs $ p
        rs' = filter ((/= jbStep done) . rStep) rs
        canDoNow = filter (not . (`isBlocked` rs') . jbStep) todo
        willDoNow = take (plSize p - (length . plJobs) p + 1) canDoNow
        todo' = filter (`notElem` willDoNow) todo
        t = jbTimeLeft done
        p' =
          p
            { plJobs =
                sort $
                  willDoNow
                    ++ (map (\j -> j {jbTimeLeft = jbTimeLeft j - jbTimeLeft done}) . filter (/= done) . plJobs) p
            }

part2Solution :: [Rule] -> Int
part2Solution rs = assembleSleigh rs (Pool 5 []) 60
