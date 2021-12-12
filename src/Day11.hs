{-# LANGUAGE TupleSections #-}
module Day11 ( solution ) where

import Common (Solution(Solution), NoSolution(..), parseArray, showArray, readNum', inArrayBounds)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Debug.Trace
import Data.Array (Array, assocs, (//), (!), accum)

solution = Solution "day11" "Dumbo Octopus" run

run input = let
    grid = parseArray readOctopus input
    in (howManyFlashes 100 grid, whenAllFlashed grid)

data Octopus = Value Int | JustFlashed
instance Show Octopus where
    show JustFlashed = "."
    show (Value v)
        | v < 10     = show v
        | otherwise  = "x"

incrementOctopus :: Octopus -> Octopus
incrementOctopus octopus = case octopus of
    Value v -> Value $ v + 1
    JustFlashed -> JustFlashed

resetFlashed :: Octopus -> Octopus
resetFlashed octopus = case octopus of
    Value v -> Value v
    JustFlashed -> Value 0

isOctopusAboutToFlash :: Octopus -> Bool
isOctopusAboutToFlash octopus = case octopus of
    Value v -> v > 9
    JustFlashed -> False

justFlushed octopus = case octopus of
    JustFlashed -> True
    _           -> False

type Grid = Array (Int, Int) Octopus

data Iteration = Iteration {
    iterationArray :: Grid,
    iterationStep :: Int,
    iterationIsIntermediate :: Bool,
    iterationTotalFlashes :: Int
} deriving Show

type Point = (Int, Int)

readOctopus :: Char -> Octopus
readOctopus = Value . readNum'

howManyFlashes :: Int -> Grid -> Int
howManyFlashes steps grid = iterationTotalFlashes $ simulate grid !! steps

whenAllFlashed grid = length $ takeWhile (not . all justFlushed . iterationArray) $ simulate grid

simulate :: Grid -> [Iteration]
simulate grid = let
    steps = iterate step (Iteration grid 0 False 0)
    in filter (not . iterationIsIntermediate) steps

step :: Iteration -> Iteration
step (Iteration grid step isIntermediate totalFlashes)
    | isIntermediate = let
        aboutToFlash = map fst $ filter (isOctopusAboutToFlash . snd) $ assocs grid
        octopusesToIncrease = concatMap (neighbours grid) aboutToFlash
        grid' = grid // map (, JustFlashed) aboutToFlash
        grid'' = accum (\oct _ -> incrementOctopus oct) grid' (map (,0) octopusesToIncrease)
        isIntermediate' = not $ null aboutToFlash
        totalFlashes' = totalFlashes + length aboutToFlash
        in Iteration grid'' step isIntermediate' totalFlashes'
    | otherwise = let
        grid' = fmap (incrementOctopus . resetFlashed) grid
        step' = step + 1
        isIntermediate' = True
        in Iteration grid' step' isIntermediate' totalFlashes

neighbours :: Grid -> Point -> [Point]
neighbours grid (y, x) = filter (inArrayBounds grid) [(y, x-1), (y-1, x), (y, x+1), (y+1, x), (y+1, x+1), (y+1, x-1), (y-1, x+1), (y-1, x-1)]
