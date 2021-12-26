{-# LANGUAGE LambdaCase #-}
module Day25 where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap, Bifunctor (second, first))
import qualified Data.Map.Strict as M
import Data.List (intercalate)
import Debug.Trace (trace)

data SeaCucumber = East | South deriving (Show, Eq)
type Point = (Int, Int)
type ScMap = M.Map Point SeaCucumber

solution = Solution "day25" "" run

run input = let
    (dim, board) = parse input
    iterations = iterate (step dim) board
    nbOfStepsForsteadyState = length (takeWhile id $ zipWith (/=) iterations (tail iterations)) + 1
    in (nbOfStepsForsteadyState, NoSolution)

parse input = let
    rows = lines input
    height = length rows
    width = length $ head rows
    points = concat $ zipWith parseLine [0..] $ lines input
    parseLine y str = zipWith (\x c -> ((y,x),c)) [0..] str
    in ((height, width), M.fromList $ map (second toSeaCucumber) $ filter (isSeaCucumber . snd) $ points)

isSeaCucumber :: Char -> Bool
isSeaCucumber = flip elem "v>"

toSeaCucumber :: Char -> SeaCucumber
toSeaCucumber c = case c of
    'v' -> South
    '>' -> East
    _   -> error "invalid input"

move :: (Int, Int) -> SeaCucumber -> ScMap -> ScMap
move dim@(height, width) scType scMap = let
    possibleToMove = filter ((==scType) . snd) $ M.toList scMap
    toMove = filter canMove possibleToMove
    newPos = M.fromList $ map (first next) toMove
    canMove (pos, _) = next pos `M.notMember` scMap
    next (y,x) = case scType of
        East -> (y, (x+1) `mod` width)
        South -> ((y+1) `mod` height, x)
    in (scMap M.\\ M.fromList toMove) `M.union` newPos

step :: (Int, Int) -> ScMap -> ScMap
step dim = move dim South . move dim East

printScMap :: (Int, Int) -> ScMap -> String
printScMap (height, witdth) scMap = (intercalate "\n" $ map (\y -> map (\x -> M.findWithDefault '.' (y,x) (M.map (\case East -> '>'; South -> 'v') scMap)) [0..witdth-1]) [0..height-1]) ++ "\n"

dbg :: (Int, Int) -> [ScMap] -> Int
dbg dim [] = 0
dbg dim (x:xs) = trace (printScMap dim x) $ 1 + dbg dim xs
