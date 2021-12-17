module Day17 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

type Velocity = (Int, Int)
type Point = (Int, Int)

solution :: Solution Int Int
solution = Solution "day17" "Trick Shot" run

-- A little brute force. :)

run :: p -> (Int, Int)
run _ = let
    minX = 94
    maxX = 151
    minY = -156
    maxY = -103
    trajectories = filter (toTarget minX maxX minY maxY) [trajectoryToRange maxX minY (vx, vy) | vx <- [1..maxX], vy <- [minY..250]]
    bestHeight = maximum $ concatMap (map snd) trajectories
    in (bestHeight, length trajectories)

toTarget :: Int -> Int -> Int -> Int -> [Point] -> Bool
toTarget minX maxX minY maxY = any (\(x, y) -> x >= minX && x <= maxX && y >= minY && y <= maxY)

trajectoryToRange :: Int -> Int -> Velocity -> [(Int, Int)]
trajectoryToRange maxX minY = takeWhile (\(x, y) -> x <= maxX && y >= minY) . trajectory

trajectory :: Velocity -> [Point]
trajectory velocity = map snd $ iterate step (velocity, (0, 0)) where
    step ((v_x, v_y), (x, y)) = ((max (v_x-1) 0, v_y-1), (x + v_x, y + v_y))
