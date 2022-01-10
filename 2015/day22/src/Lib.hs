module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Function (on)
import Data.List (minimumBy)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

type Cost = Int

type Hits = Int

type Damage = Int

type Armor = Int

type Mana = Int

type Time = Int

data Boss = Boss Hits Damage deriving (Eq, Show)

instance Read Boss where
  readPrec = do
    (lift . P.string) "Hit Points: "
    hits <- readPrec
    (lift . P.string) "\nDamage: "
    damage <- readPrec
    (lift . P.char) '\n'
    return $ Boss hits damage

data Player = Player Hits Armor Mana Mana deriving (Eq, Show)

mkHero = Player 50 0 500 0

data Spell = Spell Cost String Time Damage Armor Hits Mana deriving (Eq, Show)

spellbook =
  [ Spell 53 "Magic Missile" 0 4 0 0 0,
    Spell 73 "Drain" 0 2 0 2 0,
    Spell 113 "Shield" 6 0 7 0 0,
    Spell 173 "Poison" 6 3 0 0 0,
    Spell 229 "Recharge" 5 0 0 0 101
  ]

data Status = P | B | NF deriving (Eq, Show)

data Battle = Battle Status Int Player Boss [Spell] [(Int, Spell)] deriving (Eq, Show)

manaSpent (Battle _ _ (Player _ _ _ pspent) _ _ _) = pspent

battleStatus (Battle status _ _ _ _ _) = status

applySpell :: Spell -> Battle -> Battle
applySpell _ b@(Battle P _ _ _ _ _) = b
applySpell _ b@(Battle B _ _ _ _ _) = b
applySpell
  (Spell cost name time damage armor hits mana)
  (Battle NF turn (Player phits parmor pmana pspent) (Boss bhits bdamage) spells history) =
    Battle
      status
      turn
      (Player phits' parmor' pmana' pspent)
      (Boss bhits' bdamage)
      (if time > 1 then Spell cost name (time - 1) damage armor hits mana : spells else spells)
      history
    where
      phits' = phits + hits
      pmana' = pmana + mana
      bhits' = bhits - damage
      parmor' = if time == 1 then parmor - armor else parmor
      status = if bhits' > 0 then NF else P

addSpell :: Spell -> Battle -> Battle
addSpell _ b@(Battle P _ _ _ _ _) = b
addSpell _ b@(Battle B _ _ _ _ _) = b
addSpell
  s@(Spell cost name time damage armor hits mana)
  (Battle NF turn (Player phits parmor pmana pspent) (Boss bhits bdamage) spells history) =
    Battle
      status
      turn
      (Player phits' parmor' pmana' pspent')
      (Boss bhits' bdamage)
      (if time > 1 then Spell cost name time damage armor hits mana : spells else spells)
      ((turn - 1, s) : history)
    where
      phits' = if time == 0 then phits + hits else phits
      parmor' = parmor + armor
      pmana' = pmana - cost
      bhits' = if time == 0 then bhits - damage else bhits
      pspent' = pspent + cost
      status
        | pmana' <= 0 = B
        | phits' <= 0 = P
        | otherwise = NF

applySpells :: Battle -> Battle
applySpells b@(Battle P _ _ _ _ _) = b
applySpells b@(Battle B _ _ _ _ _) = b
applySpells (Battle NF turn p b spells history) = foldl (flip applySpell) (Battle NF (turn + 1) p b [] history) spells

isActiveSpell :: Spell -> Battle -> Bool
isActiveSpell (Spell _ name _ _ _ _ _) (Battle _ _ _ _ spells _) = name `elem` map (\(Spell _ name' _ _ _ _ _) -> name') spells

bossHits :: Battle -> Battle
bossHits b@(Battle P _ _ _ _ _) = b
bossHits b@(Battle B _ _ _ _ _) = b
bossHits (Battle NF turn (Player phits parmor pmana pspent) (Boss bhits bdamage) spells history) =
  Battle status turn (Player phits' parmor pmana pspent) (Boss bhits bdamage) spells history
  where
    phits' = phits - if bdamage <= parmor then 1 else bdamage - parmor
    status = if phits' <= 0 then B else NF

turn :: (Battle -> Battle) -> Battle -> [Battle]
turn _ b@(Battle P _ _ _ _ _) = [b]
turn _ b@(Battle B _ _ _ _ _) = [b]
turn preTurn b@(Battle NF turn _ _ _ _)
  | even turn = bp
  | otherwise = bb
  where
    b' = if even turn then (applySpells . preTurn) b else applySpells b
    bb = [bossHits b']
    bp = [addSpell s b' | s <- spellbook, not (isActiveSpell s b')]

play :: (Battle -> Battle) -> Battle -> [Battle]
play preTurn b
  | battleStatus b /= NF = [b]
  | otherwise = concatMap (play preTurn) . turn preTurn $ b

solution :: (Battle -> Battle) -> Boss -> (Battle, Mana)
solution preTurn boss =
  minimumBy (compare `on` snd)
    . map (\b -> (b, manaSpent b))
    . filter (\(Battle status _ _ _ _ _) -> status == P)
    . play preTurn
    $ Battle NF 0 mkHero boss [] []

normalDifficulty = id

part1Solution :: Boss -> (Battle, Mana)
part1Solution = solution normalDifficulty

hardDifficulty :: Battle -> Battle
hardDifficulty b@(Battle P _ _ _ _ _) = b
hardDifficulty b@(Battle B _ _ _ _ _) = b
hardDifficulty (Battle NF turn (Player phits parmor pmana pspent) boss spells history) =
  Battle status turn (Player phits' parmor pmana pspent) boss spells history
  where
    phits' = phits - 1
    status = if phits' <= 0 then B else NF

part2Solution :: Boss -> (Battle, Mana)
part2Solution = solution hardDifficulty
