module Lib
  ( part1Solution,
    part2Solution,
  )
where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data Obj = Bot Int | Output Int deriving (Eq, Ord, Show)

isBot :: Obj -> Bool
isBot (Bot _) = True
isBot _ = False

instance Read Obj where
  readPrec =
    ( do
        lift . P.string $ "bot "
        Bot <$> readPrec
    )
      PC.<++ ( do
                 lift . P.string $ "output "
                 Output <$> readPrec
             )

data Rule = Rule Obj Obj deriving (Eq, Show)

instance Read Rule where
  readPrec = do
    lift . P.string $ "low to "
    low <- readPrec
    lift . P.string $ " and high to "
    Rule low <$> readPrec

data Instruction = Take Obj Int | Give Obj Rule deriving (Eq, Show)

isGive :: Instruction -> Bool
isGive Give {} = True
isGive _ = False

isTake :: Instruction -> Bool
isTake Take {} = True
isTake _ = False

instance Read Instruction where
  readPrec =
    ( do
        lift . P.string $ "value "
        chip <- readPrec
        lift . P.string $ " goes to "
        obj <- readPrec
        return $ Take obj chip
    )
      PC.<++ ( do
                 obj <- readPrec
                 lift . P.string $ " gives "
                 Give obj <$> readPrec
             )

newtype Objs = Objs (M.Map Obj [Int]) deriving (Eq, Show)

getBots :: Objs -> Objs
getBots (Objs mobjs) = Objs . M.filterWithKey (\obj _ -> isBot obj) $ mobjs

type Log = M.Map (Int, Int) (S.Set Obj)

data Factory = Factory Objs [Instruction] Log deriving (Eq, Show)

mkFactory :: [Instruction] -> Factory
mkFactory instrs = Factory (Objs M.empty) instrs M.empty

giveChip :: Int -> Obj -> Objs -> Maybe Objs
giveChip chip obj@(Output _) objs@(Objs mobjs) = Just . Objs . M.insertWith (++) obj [chip] $ mobjs
giveChip chip obj@(Bot _) objs@(Objs mobjs) = case obj `M.lookup` mobjs of
  Just chips
    | length chips >= 2 -> Nothing
  _ -> Just . Objs . M.insertWith (++) obj [chip] $ mobjs

applyInstr :: (Objs, Log) -> Instruction -> (Objs, Log)
applyInstr (objs@(Objs mobjs), log) (Take obj chip) = case obj `M.lookup` mobjs of
  Just chips
    | length chips >= 2 -> (objs, log)
  _ -> (Objs $ M.insertWith (++) obj [chip] mobjs, log)
applyInstr (objs@(Objs mobjs), log) (Give obj@(Bot _) (Rule objL objH)) = case obj `M.lookup` mobjs of
  Just chips
    | length chips == 2 -> applyRule chips
  _ -> (objs, log)
  where
    applyRule chips = case mbobjs' of
      Just (Objs mobjs') -> (Objs $ M.delete obj mobjs', M.insertWith S.union (minC, maxC) (S.singleton obj) log)
      _ -> (objs, log)
      where
        minC = minimum chips
        maxC = maximum chips
        mbobjs' = giveChip maxC objH =<< giveChip minC objL objs
applyInstr (obj, log) _ = (obj, log)

tick :: Factory -> Factory
tick (Factory objs instrs log) = Factory objs' (filter isGive instrs) log'
  where
    (objs', log') = foldl applyInstr (objs, log) instrs

runFactory :: Factory -> Factory
runFactory factory = doer factory (tick factory)
  where
    doer factory1@(Factory objs1 _ _) factory2@(Factory objs2 _ _)
      | getBots objs1 == getBots objs2 = factory2
      | otherwise = doer (tick factory1) (tick . tick $ factory2)

getFromLog :: (Int, Int) -> Factory -> Maybe (S.Set Obj)
getFromLog chips (Factory _ _ log) = chips `M.lookup` log

part1Solution :: [Instruction] -> Maybe (S.Set Obj)
part1Solution = getFromLog (17, 61) . runFactory . mkFactory

part2Solution :: [Instruction] -> Int
part2Solution instrs = o0 * o1 * o2
  where
    (Factory (Objs mobjs) _ _) = runFactory . mkFactory $ instrs
    o0 = product . fromMaybe [0] . M.lookup (Output 0) $ mobjs
    o1 = product . fromMaybe [0] . M.lookup (Output 1) $ mobjs
    o2 = product . fromMaybe [0] . M.lookup (Output 2) $ mobjs
