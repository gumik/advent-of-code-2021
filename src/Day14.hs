{-# LANGUAGE TupleSections #-}
module Day14 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M

type Rules = M.Map String Char
type PairsCounts = M.Map String Int

solution :: Solution Int Int
solution = Solution "day14" "Extended Polymerization" run

run :: String -> (Int, Int)
run input = let
    (template, rules) = parse input
    pairsCounts = M.fromListWith (+) $ map (,1) $ pairs template
    steps = iterate (step rules) pairsCounts
    in (minMaxDiff template $ steps !! 10, minMaxDiff template $ steps !! 40)

parse :: String -> (String, Rules)
parse input = let
    [template, rulesStr] = splitOn "\n\n" input
    rules = M.fromList $ parseRules rulesStr
    parseRules = map (((!!0) &&& (head . (!!2))) . splitOn " ") . lines
    in (template, rules)

minMaxDiff :: String -> PairsCounts -> Int
minMaxDiff template pairsCounts = let
    pairsCounts' = M.insertWith (+) [head template, last template] 1 pairsCounts
    sums = map snd $ M.toList $ M.fromListWith (+) $ concatMap (\(k, v) -> [(head k, v), (k!!1, v)]) $ M.toList pairsCounts'
    min = minimum sums
    max = maximum sums
    in (max - min) `div` 2

pairs :: String -> [String]
pairs template = case template of
    (x1:x2:xs) -> [x1, x2] : pairs (x2:xs)
    _          -> []

step :: Rules -> PairsCounts -> PairsCounts
step rules pairsCounts = M.fromListWith (+) $ concatMap (\(pair, cnt) -> map (,cnt) $ extendPair rules pair) $ M.toList pairsCounts

extendPair :: Rules -> String -> [String]
extendPair rules pair = case pair of
    [x1, x2] -> [[x1, mid], [mid, x2]] where mid = rules M.! pair
    _        -> []
