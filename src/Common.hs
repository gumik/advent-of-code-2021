module Common (Solution(..), NoSolution(..), listOfNumbers, (!?), readNum) where
import Numeric (readInt)

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
