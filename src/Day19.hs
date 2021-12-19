module Day19 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

solution = Solution "day19" "" run

run input = let
    scanners = parse input
    in (scannersa, NoSolution)

parse = map parseScanner . splitOn "\n\n" where
    parseScanner = parseLine . tail . lines
    parseLine = map readNum . splitOn ","