module Common (Solution(..), NoSolution(..), listOfNumbers, (!?), readNum, parseComaSeparatedNums, toDecimal) where
import Numeric (readInt)
import Data.List.Split (splitOn)

data Solution a b = Solution {
    solutionName :: String,
    solutionDescription :: String,
    solutionRun :: String -> (a, b)
}

data NoSolution = NoSolution
instance Show NoSolution where
    show NoSolution = "(no solution)"

listOfNumbers :: String -> [Int]
listOfNumbers content = map read (lines content) :: [Int]

(!?) :: [a] -> Int -> Maybe a
xs !? n = if n >= 0 && n < length xs
    then Just $ xs !! n
    else Nothing

readNum :: String -> Int
readNum = read

parseComaSeparatedNums :: String -> [Int]
parseComaSeparatedNums = map readNum . splitOn ","

toDecimal :: Int -> [Int] -> Int 
toDecimal nary digits = sum $ zipWith (\d c -> d * nary^c) (reverse digits) [0..]
