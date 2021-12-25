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

parse input = let
    rows = lines input
    height = length rows
    width = length $ head rows
    points = concat $ zipWith parseLine [0..] $ lines input
    parseLine y str = zipWith (\x c -> ((y,x),c)) [0..] str
    in ((height, width), M.fromList $ map (second toSeaCucumber) $ filter (isSeaCucumber . snd) $ points)
    
isSeaCucumber c = any (==c) "v>"

toSeaCucumber c = case c of
    'v' -> South
    '>' -> East
    _   -> error "invalid input"

