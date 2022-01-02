module Lib
  ( part1Solution,
    part2Solution,
    part1Solution',
    part2Solution',
    initData,
    mkSchool,
  )
where

import Data.Map (Map (..), elems, empty, fromList, singleton, unionWith)
import qualified Data.Map as M

addLantern :: Map Int Int -> Map Int Int -> Map Int Int
addLantern = unionWith (+)

initData :: [Int] -> Map Int Int
initData = foldl (\m x -> m `addLantern` singleton x 1) empty

dayByDay :: Int -> Map Int Int -> Int
dayByDay 0 = sum . elems
dayByDay n =
  dayByDay (n - 1)
    . M.foldlWithKey
      ( \m d x ->
          if d == 0
            then m `addLantern` (singleton 6 x `addLantern` singleton 8 x)
            else m `addLantern` singleton (d - 1) x
      )
      empty

part1Solution :: Map Int Int -> Int
part1Solution = dayByDay 80

part2Solution :: Map Int Int -> Int
part2Solution = dayByDay 256

-- dumb solution:
data School = School Int Int Int Int Int Int Int Int Int

addToSchool :: Int -> School -> School
addToSchool 0 (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = School (d0 + 1) d1 d2 d3 d4 d5 d6 d7 d8
addToSchool 1 (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = School d0 (d1 + 1) d2 d3 d4 d5 d6 d7 d8
addToSchool 2 (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = School d0 d1 (d2 + 1) d3 d4 d5 d6 d7 d8
addToSchool 3 (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = School d0 d1 d2 (d3 + 1) d4 d5 d6 d7 d8
addToSchool 4 (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = School d0 d1 d2 d3 (d4 + 1) d5 d6 d7 d8
addToSchool 5 (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = School d0 d1 d2 d3 d4 (d5 + 1) d6 d7 d8
addToSchool 6 (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = School d0 d1 d2 d3 d4 d5 (d6 + 1) d7 d8
addToSchool 7 (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = School d0 d1 d2 d3 d4 d5 d6 (d7 + 1) d8
addToSchool 8 (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = School d0 d1 d2 d3 d4 d5 d6 d7 (d8 + 1)
addToSchool _ school = school

mkSchool :: [Int] -> School
mkSchool = foldl (flip addToSchool) (School 0 0 0 0 0 0 0 0 0)

count :: School -> Int
count (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = d0 + d1 + d2 + d3 + d4 + d5 + d6 + d7 + d8

live :: School -> [Int]
live (School d0 d1 d2 d3 d4 d5 d6 d7 d8) = count school : live school
  where
    school = School d1 d2 d3 d4 d5 d6 (d7 + d0) d8 d0

part1Solution' :: School -> Int
part1Solution' = (!! 79) . live

part2Solution' :: School -> Int
part2Solution' = (!! 255) . live
