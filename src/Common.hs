module Common (Solution(..), NoSolution(..), listOfNumbers) where

data Solution a b = Solution {
    solutionName :: String,
    solutionRun :: String -> (a, b)
}

data NoSolution = NoSolution
instance Show NoSolution where
    show NoSolution = "(no solution)"

listOfNumbers :: String -> [Int]
listOfNumbers content = map read (lines content) :: [Int]
