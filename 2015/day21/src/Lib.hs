module Lib
  ( part1Solution,
    part2Solution,
  )
where

import Data.Function (on)
import Data.List (maximumBy, minimumBy)
import Debug.Trace (trace)
import qualified Text.ParserCombinators.ReadP as P
import qualified Text.ParserCombinators.ReadPrec as PC
import Text.Read

data ItemType = Weapon | Armor | Ring deriving (Eq, Show)

type Cost = Int

data Item = Item String ItemType Cost Damage Armor deriving (Eq, Show)

cost (Item _ _ c _ _) = c

type Hits = Int

type Armor = Int

type Damage = Int

type Spent = Int

data Person = Person Hits Damage Armor [Item] deriving (Show)

instance Read Person where
  readPrec = do
    (lift . P.string) "Hit Points: "
    hits <- readPrec
    (lift . P.char) '\n'
    (lift . P.string) "Damage: "
    damage <- readPrec
    (lift . P.char) '\n'
    (lift . P.string) "Armor: "
    armor <- readPrec
    (lift . P.char) '\n'
    return $ Person hits damage armor []

mkHero :: Person
mkHero = Person 100 0 0 []

isWinner :: Person -> Person -> Bool
isWinner (Person hits1 damage1 armor1 _) (Person hits2 damage2 armor2 _) = turns1 <= turns2
  where
    d1 = if damage1 <= armor2 then 1 else damage1 - armor2
    d2 = if damage2 <= armor1 then 1 else damage2 - armor1
    turns1 = (hits2 + hits2 `mod` d1) `div` d1
    turns2 = (hits1 + hits1 `mod` d2) `div` d2

shopWeapon =
  [ Item "Dagger" Weapon 8 4 0,
    Item "Shortsword" Weapon 10 5 0,
    Item "Warhamer" Weapon 25 6 0,
    Item "Longsword" Weapon 40 7 0,
    Item "Greataxe" Weapon 74 8 0
  ]

shopArmor =
  [ Item "NoArmor" Armor 0 0 0,
    Item "Leather" Armor 13 0 1,
    Item "Chainmail" Armor 31 0 2,
    Item "Splintmail" Armor 53 0 3,
    Item "Bandedmail" Armor 75 0 4,
    Item "Platemail" Armor 102 0 5
  ]

shopRings =
  [ Item "NoRing" Ring 0 0 0,
    Item "Damage +1" Ring 25 1 0,
    Item "Damage +2" Ring 50 2 0,
    Item "Damage +3" Ring 100 3 0,
    Item "Defense +1" Ring 20 0 1,
    Item "Defense +2" Ring 40 0 2,
    Item "Defense +3" Ring 80 0 3
  ]

itemName (Item name _ _ _ _) = name

buy :: Item -> Person -> Person
buy i@(Item _ _ c d a) (Person hits damage armor items) = Person hits (damage + d) (armor + a) (i : items)

spent :: Person -> Int
spent (Person _ _ _ items) = sum . map cost $ items

part1Solution :: Person -> (Person, Int)
part1Solution boss =
  (\p -> (p, spent p))
    . minimumBy
      (compare `on` spent)
    $ [ hero
        | w <- shopWeapon,
          a <- shopArmor,
          r1 <- shopRings,
          r2 <- shopRings,
          itemName r1 == "NoRing" || r1 /= r2,
          let hero = buy w . buy a . buy r1 . buy r2 $ mkHero,
          isWinner hero boss
      ]

part2Solution :: Person -> (Person, Int)
part2Solution boss =
  (\p -> (p, spent p))
    . maximumBy
      (compare `on` spent)
    $ [ hero
        | w <- shopWeapon,
          a <- shopArmor,
          r1 <- shopRings,
          r2 <- shopRings,
          itemName r1 == "NoRing" || r1 /= r2,
          let hero = buy w . buy a . buy r1 . buy r2 $ mkHero,
          not (isWinner hero boss)
      ]
