module Lib (part1Solution) where

import Data.Char (isAlpha)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Action = Action {actionWrite :: Bool, actionMove :: Bool, actionNext :: String} deriving (Eq, Show)

instance Read Action where
  readPrec = do
    lift P.skipSpaces
    lift . P.string $ "- Write the value"
    lift P.skipSpaces
    val <- fmap (== '1') . lift $ P.get
    lift . P.char $ '.'
    lift P.skipSpaces
    lift . P.string $ "- Move one slot to the"
    lift P.skipSpaces
    move <- lift $ (P.string "right" >> return True) P.<++ (P.string "left" >> return False)
    lift . P.char $ '.'
    lift P.skipSpaces
    lift . P.string $ "- Continue with state"
    lift P.skipSpaces
    state <- lift . P.munch1 $ isAlpha
    lift . P.char $ '.'
    lift P.skipSpaces
    return $ Action val move state

data State = State {stIf0 :: Action, stIf1 :: Action} deriving (Eq, Show)

instance Read State where
  readPrec = do
    lift P.skipSpaces
    lift . P.string $ "If the current value is 0:"
    lift P.skipSpaces
    action0 <- readPrec
    lift P.skipSpaces
    lift . P.string $ "If the current value is 1:"
    action1 <- readPrec
    lift P.skipSpaces
    return $ State action0 action1

data TMachine = TMachine
  { tmTape :: M.Map Int Bool,
    tmPos :: Int,
    tmStates :: M.Map String State,
    tmState :: String,
    tmDiag :: Int
  }
  deriving (Eq, Show)

instance Read TMachine where
  readPrec = do
    lift P.skipSpaces
    lift . P.string $ "Begin in state"
    lift P.skipSpaces
    state <- lift . P.munch1 $ isAlpha
    lift . P.char $ '.'
    lift P.skipSpaces
    lift . P.string $ "Perform a diagnostic checksum after"
    lift P.skipSpaces
    diag <- readPrec
    lift P.skipSpaces
    lift . P.string $ "steps."
    lift P.skipSpaces
    states <- fmap M.fromList . lift . P.many $ do
      P.skipSpaces
      P.string "In state "
      P.skipSpaces
      st <- P.munch1 isAlpha
      P.string ":"
      P.skipSpaces
      a <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      return (st, a)
    return $ TMachine M.empty 0 states state diag

writeTM :: Bool -> TMachine -> TMachine
writeTM v tm = tm {tmTape = M.insert (tmPos tm) v . tmTape $ tm}

moveTM :: Bool -> TMachine -> TMachine
moveTM d tm = tm {tmPos = tmPos tm + if d then 1 else (-1)}

gotoTM :: String -> TMachine -> TMachine
gotoTM state tm = tm {tmState = state}

applyAction :: Action -> TMachine -> TMachine
applyAction a = gotoTM (actionNext a) . moveTM (actionMove a) . writeTM (actionWrite a)

runStep :: TMachine -> TMachine
runStep tm = case (tmState tm `M.lookup` tmStates tm, fromMaybe False $ tmPos tm `M.lookup` tmTape tm) of
  (Just st, False) -> applyAction (stIf0 st) tm
  (Just st, True) -> applyAction (stIf1 st) tm
  _ -> error "something is completely wrong"

part1Solution :: TMachine -> Int
part1Solution = length . filter id . M.elems . tmTape . snd . head . filter (\(i, tm) -> i == tmDiag tm) . zip [0 ..] . iterate runStep
