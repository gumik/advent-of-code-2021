{-# LANGUAGE TupleSections #-}
module Day05 ( solution ) where

import Data.List.Split ( splitOneOf )
import qualified Data.Map.Strict as M
import Common ( Solution(Solution) )

solution :: Solution Int Int
solution = Solution "day05" "Hydrothermal Venture" run

run :: String -> (Int, Int)
run input = let
    parsed = parse input
    in (countOverlaps $ filter isStraight parsed, countOverlaps parsed)

type Point = (Int, Int)
type Line = (Point, Point)

parse :: String -> [Line]
parse = map parseLine . lines where
  parseLine = toPoints . map read . filter (not . null) . splitOneOf ",-> "
  toPoints [a, b, c, d] = ((a, b), (c, d))

isStraight :: Line -> Bool
isStraight ((a, b), (c, d)) = a == c || b == d

pointsInStraightLine :: Line -> [Point]
pointsInStraightLine ((a, b), (c, d)) = [(x, y) | x <- range a c, y <- range b d]

pointsInDiagonalLine :: Line -> [Point]
pointsInDiagonalLine ((a, b), (c, d)) = take (abs (a-c) + 1) $ iterate (add (dir a c) (dir b d)) (a, b)

dir :: Int -> Int -> Int
dir a b = if a > b then -1 else 1

add :: Int -> Int -> (Int, Int) -> (Int, Int)
add d1 d2 (x, y) = (x+d1, y+d2)

range :: Int -> Int -> [Int]
range a b = if a > b then [b..a] else [a..b]

affectedPoints :: [Line] -> [Point]
affectedPoints = concatMap choosePointsInLine where
    choosePointsInLine line = if isStraight line then pointsInStraightLine line else pointsInDiagonalLine line

countOverlaps :: [Line] -> Int
countOverlaps = M.size . M.filter (>1) . M.fromListWith (+) . map (, 1) . affectedPoints
