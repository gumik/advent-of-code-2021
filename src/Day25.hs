module Day25 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

solution = Solution "day25" "" run

run input = let
    board = parse input
    in (board, NoSolution)

parse = filter (isSeaCucumber . snd) . concat . zipWith parseLine [0..] . lines where
    parseLine y str = zipWith (\x c -> ((y,x),c)) [0..] str
    
isSeaCucumber c = case c of
    'v' -> True
    '>' -> True
    _   -> False