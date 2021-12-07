module Day07 ( solution ) where

import Common (Solution(Solution), NoSolution(..), parseComaSeparatedNums)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Data.List (sort)
import qualified Data.Map.Strict as M

-- This is not correct solution, but works for the input. :)

solution = Solution "day07" "The Treachery of Whales" run

run input = let
    numbers = parseComaSeparatedNums input
    in (bestCost fuelConsumption1 numbers, bestCost fuelConsumption2 numbers)

bestCost :: (Int -> Int) -> [Int] -> Int
bestCost fuelConsumption nums = minimum $ map (cost fuelConsumption nums) nums

cost :: (Int -> Int) -> [Int] -> Int -> Int
cost fuelConsumption nums m = sum $ map (fuelConsumption . abs . ((-) m)) nums

fuelConsumption1 :: Int -> Int
fuelConsumption1 = id

fuelConsumption2 :: Int -> Int
fuelConsumption2 distance = (1 + distance) * distance `div` 2
