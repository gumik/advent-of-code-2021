module Main where

import System.Environment ( getArgs )
import Day01 ( solution )
import Day02 ( solution )
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
    stringSolution Day02.solution]

stringSolution :: (Show a1, Show a2) => Solution a1 a2 -> Solution String String
stringSolution (Solution name desc run) = Solution name desc (bimap show show . run)
