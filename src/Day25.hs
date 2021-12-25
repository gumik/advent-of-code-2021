module Day25 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap, Bifunctor (second))
import qualified Data.Map.Strict as M

data SeaCucumber = East | South deriving Show

solution = Solution "day25" "" run

run input = let
    board = parse input
    in (board, NoSolution)

parse = M.fromList . map (second toSeaCucumber) . filter (isSeaCucumber . snd) . concat . zipWith parseLine [0..] . lines where
    parseLine y str = zipWith (\x c -> ((y,x),c)) [0..] str

isSeaCucumber c = any (==c) "v>"

toSeaCucumber c = case c of
    'v' -> South
    '>' -> East
    _   -> error "invalid input"
