module Day01 ( solution ) where

import Common (Solution(Solution), listOfNumbers)

solution = Solution "day01" run

-- run :: IO ()
-- run = do
--     content <- readFile "data/day01-input.txt"
--     let input = map read (lines content) :: [Int]
--         output = increasedMeasurements input
--         output2 = increasedThreeMeasurementsWindow input
--     print output
--     print output2

run :: String -> (Int, Int)
run input = (increasedMeasurements numbers, increasedThreeMeasurementsWindow numbers) where
    numbers = listOfNumbers input

increasedMeasurements :: [Int] -> Int
increasedMeasurements l = length $ filter (> 0) $ zipWith (-) (tail l) l

increasedThreeMeasurementsWindow :: [Int] -> Int
increasedThreeMeasurementsWindow l = increasedMeasurements windows where
    windows = zipWith3 (\a b c -> a + b + c) l l2 l3
    l2 = tail l
    l3 = tail l2
