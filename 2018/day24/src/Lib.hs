module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Char (isAlpha)
import Data.Function (on)
import Data.List (foldl', minimumBy, sortBy)
import Data.Map (Map (..))
import qualified Data.Map as M
import Data.Ord (Ordering (..), compare)
import Debug.Trace (trace)
import Text.ParserCombinators.ReadP (skipSpaces)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data AType = Immune | Infection deriving (Eq, Ord, Show)

data Army = Army
  { aId :: Int,
    aType :: AType,
    aUnits :: Int,
    aHP :: Int,
    aWeak :: [String],
    aImmune :: [String],
    aAttack :: String,
    aDamage :: Int,
    aInit :: Int
  }
  deriving (Eq, Show)

readCommaList :: P.ReadP [String]
readCommaList =
  P.many1 $ do
    w <- P.munch1 isAlpha
    P.optional . P.string $ ", "
    return w

instance Read Army where
  readPrec = do
    units <- readPrec
    lift . P.string $ " units each with "
    hp <- readPrec
    lift . P.string $ " hit points "
    (weaks, immunes) <-
      lift . P.option ([], []) $
        ( do
            P.char '('
            ws <- P.option [] (P.string "weak to " >> readCommaList)
            P.optional . P.char $ ';'
            P.skipSpaces
            is <- P.option [] (P.string "immune to " >> readCommaList)
            P.char ')'
            return (ws, is)
        )
          P.<++ ( do
                    P.char '('
                    is <- P.option [] (P.string "immune to " >> readCommaList)
                    P.optional . P.char $ ';'
                    P.skipSpaces
                    ws <- P.option [] (P.string "weak to " >> readCommaList)
                    P.char ')'
                    return (ws, is)
                )
    lift P.skipSpaces
    lift . P.string $ "with an attack that does "
    damage <- readPrec
    lift P.skipSpaces
    attack <- lift . P.munch1 $ isAlpha
    lift P.skipSpaces
    lift . P.string $ "damage at initiative "
    Army 0 Immune units hp weaks immunes attack damage <$> readPrec

newtype Fight = Fight {unFight :: Map Int Army} deriving (Eq, Show)

instance Read Fight where
  readPrec = do
    lift . P.string $ "Immune System:"
    lift P.skipSpaces
    immns <- lift . P.many $ do
      a <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      return a
    lift . P.string $ "Infection:"
    lift P.skipSpaces
    infs <- lift . P.many $ do
      a <- PC.readPrec_to_P readPrec 0
      P.skipSpaces
      return a
    return . Fight . M.fromAscList . zipWith (\i a -> (i, a {aId = i})) [0 ..] $
      (map (\a -> a {aType = Immune}) immns ++ map (\a -> a {aType = Infection}) infs)

damageEstimation :: Army -> Army -> Int
damageEstimation attacker defender
  | aAttack attacker `elem` aImmune defender = 0
  | aAttack attacker `elem` aWeak defender = 2 * damage
  | otherwise = damage
  where
    damage = aUnits attacker * aDamage attacker

effectivePower :: Army -> Int
effectivePower a = aUnits a * aDamage a

damageApplied :: Army -> Army -> Maybe Army
damageApplied attacker defender
  | attackerDamage > aUnits defender * aHP defender = Nothing
  | otherwise = Just defender {aUnits = aUnits defender - (attackerDamage `div` aHP defender)}
  where
    attackerDamage = damageEstimation attacker defender

attackerTargetSelectionCmp :: Army -> Army -> Ordering
attackerTargetSelectionCmp a1 a2
  | ep1 > ep2 = LT
  | ep1 < ep2 = GT
  | otherwise = aInit a2 `compare` aInit a1
  where
    ep1 = effectivePower a1
    ep2 = effectivePower a2

defenderCmp :: Army -> Army -> Army -> Ordering
defenderCmp attacker defender1 defender2
  | de1 > de2 = LT
  | de1 < de2 = GT
  | otherwise = attackerTargetSelectionCmp defender1 defender2
  where
    de1 = damageEstimation attacker defender1
    de2 = damageEstimation attacker defender2

intents :: Fight -> [(Int, Int)]
intents f =
  map snd
    . sortBy (\a b -> fst b `compare` fst a)
    . fst
    . foldl' findTarget ([], (infsArmies, immnArmies))
    . sortBy attackerTargetSelectionCmp
    . M.elems
    . unFight
    $ f
  where
    infsArmies = M.filter ((== Infection) . aType) . unFight $ f
    immnArmies = M.filter ((== Immune) . aType) . unFight $ f
    findTarget (is, (infAs, imnAs)) a = case aType a of
      Infection
        | M.null imnAs -> (is, (infAs, imnAs))
        | damageEstimation a targetImmn == 0 -> (is, (infAs, imnAs))
        | otherwise -> ((aInit a, (aId a, aId targetImmn)) : is, (infAs, M.filterWithKey (\k _ -> k /= aId targetImmn) imnAs))
      Immune
        | M.null infAs -> (is, (infAs, imnAs))
        | damageEstimation a targetInf == 0 -> (is, (infAs, imnAs))
        | otherwise -> ((aInit a, (aId a, aId targetInf)) : is, (M.filterWithKey (\k _ -> k /= aId targetInf) infAs, imnAs))
      where
        targetInf = minimumBy (defenderCmp a) . M.elems $ infAs
        targetImmn = minimumBy (defenderCmp a) . M.elems $ imnAs

attack :: Fight -> [(Int, Int)] -> Fight
attack = foldl' doIntended
  where
    doIntended :: Fight -> (Int, Int) -> Fight
    doIntended f (attackerId, defenderId) = case ((M.lookup attackerId . unFight) f, (M.lookup defenderId . unFight) f) of
      (Just attacker, Just defender) -> case damageApplied attacker defender of
        Just defender' -> f {unFight = M.insert defenderId defender' . unFight $ f}
        _ -> f {unFight = M.delete defenderId . unFight $ f}
      _ -> f

fightTillTheEnd :: Fight -> Fight
fightTillTheEnd f
  | M.null infsArmies || M.null immnArmies = f
  | f' == f = f
  | otherwise = fightTillTheEnd f'
  where
    infsArmies = M.filter ((== Infection) . aType) . unFight $ f
    immnArmies = M.filter ((== Immune) . aType) . unFight $ f
    is = intents f
    f' = attack f is

scoreWinner :: Fight -> Int
scoreWinner = sum . map aUnits . M.elems . unFight . fightTillTheEnd

winner :: Fight -> Maybe AType
winner f
  | null infsArmies = Just Immune
  | null immnArmies = Just Infection
  | otherwise = Nothing
  where
    f' = fightTillTheEnd f
    infsArmies = M.filter ((== Infection) . aType) . unFight $ f'
    immnArmies = M.filter ((== Immune) . aType) . unFight $ f'

boost :: Int -> Fight -> Fight
boost n f = f {unFight = M.map (\a -> if aType a == Immune then a {aDamage = aDamage a + n} else a) . unFight $ f}

findMinimalBoost :: Fight -> Int -> Int -> Int
findMinimalBoost f from to
  | from == to = from
  | to - from == 1 && (winner . boost from) f == Just Immune = from
  | to - from == 1 = to
  | (winner . boost middle) f == Just Immune = findMinimalBoost f from middle
  | otherwise = findMinimalBoost f middle to
  where
    middle = (from + to) `div` 2

part1Solution :: Fight -> Int
part1Solution = scoreWinner . fightTillTheEnd

part2Solution :: Fight -> Int
part2Solution f = scoreWinner . fightTillTheEnd . boost (findMinimalBoost f 1 maxBoost) $ f
  where
    maxBoost = maximum . map (\a -> aUnits a * aHP a * 2) . filter ((== Infection) . aType) . M.elems . unFight $ f
