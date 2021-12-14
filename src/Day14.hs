{-# LANGUAGE TupleSections #-}
module Day14 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M

solution = Solution "day14" "" run

run input = let
    (template, rules) = parse input
    steps = iterate (step rules) template
    in (minMaxSum $ steps !! 10, 0)

parse input = let
    [template, rulesStr] = splitOn "\n\n" input
    rules = M.fromList $ parseRules rulesStr
    parseRules = map (((!!0) &&& (head . (!!2))) . splitOn " ") . lines
    in (template, rules)

step rules template = case template of
    (x1:x2:xs) -> x1:(rules M.! [x1,x2]):step rules (x2:xs)
    [x] -> [x]
    []  -> []

minMaxSum template = let
    sums = map snd $ M.toList $ M.fromListWith (+) $ map (,1) template
    min = minimum sums
    max = maximum sums
    in max - min

