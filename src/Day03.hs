module Day03 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, toDecimal)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

solution :: Solution Int Int
solution = Solution "day03" "Binary Diagnostic" run

run :: String -> (Int, Int)
run input = let
    numbersList = lines input
    positiveNegativeNumbers = map toPositiveNegative numbersList
    in (powerConsumption positiveNegativeNumbers, lifeSupportRating positiveNegativeNumbers)

type Diagnostic = [[Int]]

powerConsumption :: Diagnostic -> Int
powerConsumption input = let
    gamma = positiveNegativeToBinary $ sumNumbers input
    epsilon = invert gamma
    in toDecimal 2 gamma * toDecimal 2 epsilon

sumNumbers :: Diagnostic -> [Int]
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

lifeSupportRating :: Diagnostic -> Int
lifeSupportRating numbers = let
    oxygenGeneratorRating = rating oxygenGeneratorSelector numbers
    co2ScrubberRating = rating co2ScrubberSelector numbers
    in oxygenGeneratorRating *co2ScrubberRating

rating :: (Int -> Int -> Bool) -> Diagnostic -> Int
rating selector numbers = let
    ratingList = head $ snd $ last $ takeWhile (not . null . snd) $ iterate (step selector) (0, numbers)
    in toDecimal 2 $ positiveNegativeToBinary ratingList

oxygenGeneratorSelector :: Int -> Int -> Bool
oxygenGeneratorSelector sumValue value = if sumValue == 0 then value > 0 else sumValue * value > 0

co2ScrubberSelector :: Int -> Int -> Bool
co2ScrubberSelector sumValue value = if sumValue == 0 then value < 0 else sumValue * value < 0

step :: (Int -> Int -> Bool) -> (Int, Diagnostic) -> (Int, Diagnostic)
step selectFunction (pos, numbers) = if pos >= length (head numbers) then (pos, []) else let
    summed = sumNumbers numbers
    valueOnPos = summed !! pos
    numbers' = filter (selectFunction valueOnPos . (!! pos)) numbers
    in (pos + 1, numbers')
