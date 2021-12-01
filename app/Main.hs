module Main where

import System.Environment
import Day01
import Day02
import Common (Solution(..))
import Data.Bifunctor (bimap)

main :: IO ()
main = do
    args <- getArgs
    let solution = head $ filter ((== head args) . solutionName) solutions
        name = solutionName solution
        inputFile = "data/" ++ name ++ "-input.txt"

    input <- readFile inputFile

    putStrLn name
    let (output1, output2) = solutionRun solution input
    putStrLn $ "    " ++ output1
    putStrLn $ "    " ++ output2

solutions = [
    stringSolution Day01.solution,
    stringSolution Day02.solution]

stringSolution (Solution name run) = Solution name (fmap (bimap show show) run)
