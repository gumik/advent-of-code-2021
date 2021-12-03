module Day03 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

solution = Solution "day03" "Binary Diagnostic" run

run input = let
    numbersList = lines input
    positiveNegativeNumbers = map toPositiveNegative numbersList
    in (powerConsumption positiveNegativeNumbers, lifeSupportRating positiveNegativeNumbers)

-- powerConsumption :: [String] -> [Int]
powerConsumption input = let
    gamma = positiveNegativeToBinary $ sumNumbers input
    epsilon = invert gamma
    in toDecimal gamma * toDecimal epsilon

sumNumbers = foldl1 sumArrays

toPositiveNegative :: String -> [Int]
toPositiveNegative = map charToValue where
    charToValue c = case c of
        '0' -> -1
        '1' -> 1
        _   -> error "Invalid input"

sumArrays :: [Int] -> [Int] -> [Int]
sumArrays = zipWith (+)

positiveNegativeToBinary :: [Int] -> [Int]
positiveNegativeToBinary = map valueToBinary where
    valueToBinary value = if value < 0 then 0 else 1

invert :: [Int] -> [Int]
invert = map invertValue where
    invertValue value = if value == 0 then 1 else 0

toDecimal :: [Int] -> Int
toDecimal l = foldl calc 0 $ zip [0..] (reverse l) where
    calc value (coeff, x) = value + 2^coeff * x


lifeSupportRating numbers = let
    oxygenGeneratorRating = rating oxygenGeneratorSelector numbers
    co2ScrubberRating = rating co2ScrubberSelector numbers
    in oxygenGeneratorRating *co2ScrubberRating

rating selector numbers = let 
    ratingList = head $ snd $ last $ takeWhile (not . null . snd) $ iterate (step selector) (0, numbers)
    in toDecimal $ positiveNegativeToBinary ratingList
-- last $ take (length (head numbers))

oxygenGeneratorSelector sumValue value = if sumValue == 0 then value > 0 else sumValue * value > 0

co2ScrubberSelector sumValue value = if sumValue == 0 then value < 0 else sumValue * value < 0

step selectFunction (pos, numbers) = if pos >= length (head numbers) then (pos, []) else let
    summed = sumNumbers numbers
    valueOnPos = summed !! pos
    numbers' = filter (selectFunction valueOnPos . (!! pos)) numbers
    in (pos + 1, numbers')