{-# LANGUAGE TupleSections #-}
module Day06 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Data.List (iterate', sort)
import qualified Data.Map.Strict as M

solution :: Solution Int Int
solution = Solution "day06" "Lanternfish" run

run :: String -> (Int, Int)
run inputStr = let
    input = parse inputStr
    in (howManyFish 80 input, howManyFish 256 input)

parse :: String -> [Int]
parse = map readNum . splitOn ","

howManyFish :: Int -> [Int] -> Int
howManyFish days population = sum $ simulate days (slotsFromPopulation population)

slotsFromPopulation :: [Int] -> [Int]
slotsFromPopulation = map snd . sort . M.toList . M.unionWith (+) emptySlots . M.fromListWith (+) . map (,1) where
    emptySlots = M.fromList [(i, 0) | i <- [0..8]]

simulate :: Int -> [Int] -> [Int]
simulate days slots = iterate simulateDay slots !! days

simulateDay :: [Int] -> [Int]
simulateDay slots = let
    [d0, d1, d2, d3, d4, d5, d6, d7, d8] = slots
    in [d1, d2, d3, d4, d5, d6, d7 + d0, d8, d0]
