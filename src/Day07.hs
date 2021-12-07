module Day07 ( solution ) where

import Common (Solution(Solution), NoSolution(..), parseComaSeparatedNums)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Data.List (sort)

solution :: Solution Int Int
solution = Solution "day07" "The Treachery of Whales" run

run :: String -> (Int, Int)
run input = let
    numbers = parseComaSeparatedNums input
    in (bestCost numbers, bestCost2 numbers)

bestCost :: [Int] -> Int
bestCost nums = let
    c1 = cost fuelConsumption1 nums (median nums)
    c2 = cost fuelConsumption1 nums (median nums + 1)
    in min c1 c2

bestCost2 :: [Int] -> Int
bestCost2 nums = let
    c1 = cost fuelConsumption2 nums (average nums)
    c2 = cost fuelConsumption2 nums (average nums + 1)
    in min c1 c2

median :: Integral a => [a] -> a
median nums = let
    sorted = sort nums
    (midIdx, rest) = length nums `divMod` 2
    in if rest == 0
        then ((sorted !! midIdx)  +  (sorted !! (midIdx - 1))) `div` 2
        else sorted !! midIdx

average :: Foldable t => t Int -> Int
average nums = sum nums `div` length nums

cost :: (Num a, Num b) => (b -> a) -> [b] -> b -> a
cost fuelConsumption nums m = sum $ map (fuelConsumption . abs . (-) m) nums

fuelConsumption1 :: Int -> Int
fuelConsumption1 = id

fuelConsumption2 :: Int -> Int
fuelConsumption2 distance = (1 + distance) * distance `div` 2
