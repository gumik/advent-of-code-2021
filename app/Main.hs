module Main where

import System.Environment ( getArgs )
import Day01 ( solution )
import Day02 ( solution )
import Day03 ( solution )
import Day04 ( solution )
import Day05 ( solution )
import Day06 ( solution )
import Day07 ( solution )
import Day08 ( solution )
import Day09 ( solution )
import Day10 ( solution )
import Day11 ( solution )
import Day12 ( solution )
import Day13 ( solution )
import Day14 ( solution )
import Day15 ( solution )
import Day16 ( solution )
import Day17 ( solution )
import Day18 ( solution )
import Day19 ( solution )
import Day20 ( solution )
import Day21 ( solution )
import Day22 ( solution )
import Day23 ( solution )
import Day24 ( solution )
import Day25 ( solution )
import Common (Solution(..), (!?))
import Data.Bifunctor (bimap)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    args <- getArgs
    let daySelection = args !? 0
        otherInput = args !? 1
        solutionsToRun = filter (maybe (const True) (==) daySelection . solutionName) solutions
    mapM_ (runSolution otherInput) solutionsToRun

runSolution :: Maybe String -> Solution String String -> IO ()
runSolution otherInput solution = do
    let name = solutionName solution
        description = solutionDescription solution
        inputSuffix = fromMaybe "input" otherInput
        inputFile = "data/" ++ name ++ "-" ++ inputSuffix ++ ".txt"

    input <- readFile inputFile

    putStrLn $ name ++ " - " ++ description
    let (output1, output2) = solutionRun solution input
    putStrLn $ "    " ++ output1
    putStrLn $ "    " ++ output2

solutions :: [Solution String String]
solutions = [
    stringSolution Day01.solution,
    stringSolution Day02.solution,
    stringSolution Day03.solution,
    stringSolution Day04.solution,
    stringSolution Day05.solution,
    stringSolution Day06.solution,
    stringSolution Day07.solution,
    stringSolution Day08.solution,
    stringSolution Day09.solution,
    stringSolution Day10.solution,
    stringSolution Day11.solution,
    stringSolution Day12.solution,
    stringSolution Day13.solution,
    stringSolution Day14.solution,
    stringSolution Day15.solution,
    stringSolution Day16.solution,
    stringSolution Day17.solution,
    stringSolution Day18.solution,
    stringSolution Day19.solution,
    stringSolution Day20.solution,
    stringSolution Day21.solution,
    stringSolution Day22.solution,
    stringSolution Day23.solution,
    stringSolution Day24.solution,
    stringSolution Day25.solution]

stringSolution :: (Show a1, Show a2) => Solution a1 a2 -> Solution String String
stringSolution (Solution name desc run) = Solution name desc (bimap show show . run)
