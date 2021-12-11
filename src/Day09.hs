{-# LANGUAGE TupleSections #-}
module Day09 ( solution ) where

import Common (Solution(Solution), NoSolution(..), parseArray, readNum', inArrayBounds)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Data.Array
import qualified Data.Set as S
import Data.List (unfoldr, sort)

solution = Solution "day09" "Smoke Basin" run

run input = let
    heightMap = parseArray readNum' input
    lowPointsList = lowPoints heightMap
    in (sumOfRiskLevels heightMap lowPointsList, threeLargestBasinsMultiply heightMap lowPointsList)

type HeightMap = Array Point Int
type Point = (Int, Int)

sumOfRiskLevels :: HeightMap -> [Point] -> Int
sumOfRiskLevels heightMap lowPointsList = sum $ map (riskLevel heightMap) lowPointsList

lowPoints :: HeightMap -> [Point]
lowPoints heightMap = filter (isLowPoint heightMap) (indices heightMap) 

isLowPoint :: HeightMap -> Point -> Bool
isLowPoint heightMap point = let
    neighboursHeight = map (pointHeight heightMap) (neighbours point)
    p = pointHeight heightMap point
    in all (>p) neighboursHeight

neighbours :: Point -> [Point]
neighbours (y, x) = [(y, x-1), (y-1, x), (y, x+1), (y+1, x)]

pointHeight :: HeightMap -> Point -> Int
pointHeight heightMap (y, x)
    | inArrayBounds heightMap (y, x)  = heightMap ! (y, x)
    | otherwise                       = 10

riskLevel :: HeightMap -> Point -> Int
riskLevel heightMap yx = heightMap ! yx + 1


data Iteration = Iteration { iterationHeightMap :: HeightMap
                           , iterationQueue :: [Point]
                           , iterationPoints :: S.Set Point}
                deriving Show

threeLargestBasinsMultiply heightMap lowPoints = product $ take 3 $ reverse $ sort $ map (S.size . iterationPoints . basin heightMap) lowPoints

basin :: HeightMap -> Point -> Iteration
basin heightMap point = head $ dropWhile (not . null . iterationQueue) $ iterate step (Iteration heightMap [point] S.empty)

step :: Iteration -> Iteration
step (Iteration heightMap [] points) = Iteration heightMap [] points
step (Iteration heightMap queue points) = let
    point:queue' = queue
    neighboursList = neighbours point
    height = heightMap ! point
    higher = map fst $ filter (\(p,h) -> h > height && h < 9 && p `S.notMember` points) (map (\p -> (p,pointHeight heightMap p)) neighboursList)
    points' = point `S.insert` points
    queue'' = queue' ++ higher
    in Iteration heightMap queue'' points'