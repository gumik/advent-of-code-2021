module Day08 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, toDecimal)
import Data.List.Split (splitOn)
import Data.Bifunctor (second)
import Data.List (sort)
import qualified Data.Set as S
import qualified Data.Map.Strict as M

solution :: Solution Int Int
solution = Solution "day08" "Seven Segment Search" run

run :: String -> (Int, Int)
run inputStr = let
    input = parse inputStr
    in (easyDigitsCount input, sum $ map numberOnDisplay input)

parse :: String -> [([String], [String])]
parse = map parseLine . lines where
    parseLine = second tail . break (== "|") . words

easyDigitsCount :: [([String], [String])] -> Int
easyDigitsCount = length . filter validLen . concatMap snd where
    validLen s = length s `elem` [2, 3, 4, 7]

numberOnDisplay :: ([String], [String]) -> Int
numberOnDisplay = toDecimal 10 . digitsOnDisplay

digitsOnDisplay :: ([String], [String]) -> [Int]
digitsOnDisplay (codes, disp) = let
    characteristics = codesCharacteristic codes
    codesSorted = map sort codes
    codeToNum = M.fromList $ zipWith (\a b -> (a, characteristicToNum M.! b)) codesSorted characteristics
    dispSorted = map sort disp
    in map (codeToNum M.!) dispSorted

characteristicToNum :: M.Map (Int, Int, Int) Int
characteristicToNum = let
    codes = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]
    characteristics = codesCharacteristic codes
    in M.fromList $ zip characteristics [0..]

codesCharacteristic :: [String] -> [(Int, Int, Int)]
codesCharacteristic codes = let
    codesSets = map S.fromList codes
    in map (codeCharacteristic codesSets) codesSets

codeCharacteristic :: [S.Set Char] -> S.Set Char -> (Int, Int, Int)
codeCharacteristic codes code = let
    parents = length $ filter (code `S.isSubsetOf`) codes
    children = length $ filter (`S.isSubsetOf` code) codes
    in (S.size code, parents, children)
