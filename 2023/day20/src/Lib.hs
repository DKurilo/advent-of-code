module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlphaNum)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import Text.Read

data Queue a = Queue {qin :: [a], qout :: [a]} deriving (Eq, Show)

mkQueue :: Queue a
mkQueue = Queue [] []

push :: a -> Queue a -> Queue a
push x q = q {qin = x : qin q}

pop :: Queue a -> (Maybe a, Queue a)
pop q
  | null outs && null ins = (Nothing, q)
  | null outs = (Just . head $ outs', q {qout = tail outs', qin = []})
  | otherwise = (Just . head $ outs, q {qout = tail outs})
  where
    outs = qout q
    ins = qin q
    outs' = reverse ins

instance Foldable Queue where
  foldr f x q = foldr f x (qout q <> (reverse . qin) q)
  null q = (null . qin) q && (null . qout) q

data Pulse = PLow | PHigh deriving (Eq, Show)

data ModuleState = MOn | MOff deriving (Eq, Show)

data ModuleType = FlipFlop | Conjuction | Broadcaster deriving (Eq, Show)

data Module = Module
  { mname :: String,
    mtype :: ModuleType,
    mstate :: ModuleState,
    mlast :: [(String, Pulse)],
    mdest :: [String]
  }
  deriving (Eq, Show)

readDest :: P.ReadP [String]
readDest = P.many $ do
  cs <- P.munch1 isAlphaNum
  ( do
      _ <- P.string ", "
      return ()
    )
    P.<++ P.eof
  return cs

instance Read Module where
  readPrec =
    lift $
      ( do
          _ <- P.string "broadcaster -> "
          Module "broadcaster" Broadcaster MOn [] <$> readDest
      )
        P.<++ ( do
                  _ <- P.char '%'
                  name <- P.munch isAlphaNum
                  _ <- P.string " -> "
                  Module name FlipFlop MOff [] <$> readDest
              )
        P.<++ ( do
                  _ <- P.char '&'
                  name <- P.munch isAlphaNum
                  _ <- P.string " -> "
                  Module name Conjuction MOn [] <$> readDest
              )

adjustConjuctions :: [Module] -> [Module]
adjustConjuctions ms =
  fmap
    ( \m -> case mtype m of
        Conjuction -> m {mlast = fmap (\m' -> (mname m', PLow)) . filter ((mname m `elem`) . mdest) $ ms}
        _ -> m
    )
    ms

data System = System
  { sModules :: M.Map String Module,
    sQueue :: Queue (Pulse, (String, String)),
    sLows :: Int,
    sHighs :: Int,
    sNamedLows :: M.Map String Int
  }
  deriving (Show)

mkSystem :: [Module] -> System
mkSystem ms = System sms mkQueue 0 0 (M.map (const 0) sms)
  where
    sms = M.fromList . fmap (\m -> (mname m, m)) $ ms

isInSameState :: System -> System -> Bool
isInSameState s1 s2 = sModules s1 == sModules s2 && sQueue s1 == sQueue s2

sendPulse :: Pulse -> String -> String -> System -> System
sendPulse PLow fromName name s =
  s
    { sQueue = (push (PLow, (fromName, name)) . sQueue) s,
      sLows = sLows s + 1,
      sNamedLows = M.adjust (+ 1) name . sNamedLows $ s
    }
sendPulse PHigh fromName name s = s {sQueue = (push (PHigh, (fromName, name)) . sQueue) s, sHighs = sHighs s + 1}

processStep :: System -> System
processStep s = case mbPulseName of
  Just (p, (fromName, name)) -> case name `M.lookup` sModules s of
    Just m
      | t == FlipFlop -> case p of
          PHigh -> s'
          PLow -> foldl' (flip (sendPulse p'' name)) s''' dest
            where
              ffState = mstate m
              (ffState', p'') = if ffState == MOff then (MOn, PHigh) else (MOff, PLow)
              s''' = s' {sModules = M.insert name (m {mstate = ffState'}) . sModules $ s'}
      | t == Conjuction -> foldl' (flip (sendPulse p' name)) s'' dest
      | t == Broadcaster -> foldl' (flip (sendPulse p name)) s' dest
      where
        t = mtype m
        dest = mdest m
        m' = m {mlast = fmap (\l@(fromName', _) -> if fromName' == fromName then (fromName, p) else l) . mlast $ m}
        s'' = s' {sModules = M.insert name m' . sModules $ s'}
        p' = if (all ((== PHigh) . snd) . mlast) m' then PLow else PHigh
    _ -> s'
  _ -> s
  where
    (mbPulseName, q) = pop . sQueue $ s
    s' = s {sQueue = q}

process :: System -> System
process s
  | (null . sQueue) s = s
  | otherwise = process . processStep $ s

pushButton :: System -> System
pushButton = sendPulse PLow "button" "broadcaster"

sent :: System -> Int
sent s = sLows s * sHighs s

part1Solution :: [String] -> Int
part1Solution css
  | iLoopEnd > steps = sent . last $ ss
  | otherwise = (lowsBeforeLoop + (loopsCount * lowsInLoop) + lowsRest) * (highsBeforeLoop + (loopsCount * highsInLoop) + highsRest)
  where
    steps = 1000
    s = mkSystem . adjustConjuctions . fmap read $ css
    ss = take (steps + 1) . iterate (process . pushButton) $ s
    fromStartWithLoop =
      fmap snd
        . takeWhile
          ( \(n, s') ->
              not
                . any (\s'' -> s' `isInSameState` s'')
                . take n
                $ ss
          )
        . zip [0 ..]
        $ ss
    iLoopEnd = length fromStartWithLoop
    loopEnd = ss !! iLoopEnd
    iLoopStart = fst . head . filter ((`isInSameState` loopEnd) . snd) . zip [0 ..] $ fromStartWithLoop
    loopLength = iLoopEnd - iLoopStart
    loop = drop iLoopStart fromStartWithLoop
    sStartLoop = head loop
    lowsBeforeLoop = sLows sStartLoop
    highsBeforeLoop = sHighs sStartLoop
    lowsInLoop = sLows loopEnd - lowsBeforeLoop
    highsInLoop = sHighs loopEnd - highsBeforeLoop
    (loopsCount, iRestLast) = (steps - iLoopStart) `divMod` loopLength
    lastInRest = loop !! iRestLast
    lowsRest = sLows lastInRest - lowsBeforeLoop
    highsRest = sHighs lastInRest - highsBeforeLoop

-- cycle for rx is too high, but it's easy to notice each module
-- that contributes to rx have smaller cycle that starts for 0
-- so all calculations for part1 can be simplified, but I don't want to do it
-- and lx is only one that sends to rx
part2Solution :: [String] -> Int
part2Solution css = foldl' lcm (head named) (tail named)
  where
    s =
      mkSystem
        . adjustConjuctions
        . fmap read
        $ css
    ss = iterate (process . pushButton) s
    named = fmap (getCyleForNamed . mname) . filter (\m -> "lx" `elem` mdest m) . M.elems . sModules $ s

    getCyleForNamed :: String -> Int
    getCyleForNamed cs =
      length
        . takeWhile ((< 1) . fromMaybe (-1) . M.lookup cs . sNamedLows)
        $ ss
