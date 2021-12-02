module Day02 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

solution = Solution "day02" "Dive!" run

run :: String -> (Int, Int)
run input = let
    commands = parse input
    in (finalPosition commands, finalPositionFixed commands)

data Command = Forward | Up | Down

parse :: String -> [(Command, Int)]
parse = map parseLine . lines

parseLine :: String -> (Command, Int)
parseLine line = let
    [cmdStr, valueStr] = splitOn " " line
    value = readNum valueStr
    cmd = case cmdStr of
        "forward" -> Forward
        "up"      -> Up
        "down"    -> Down
    in (cmd, value)

finalPosition :: [(Command, Int)] -> Int
finalPosition commands = let
    (horizontal, depth) = foldl calculatePosition (0, 0) commands
    in horizontal * depth

calculatePosition :: (Int, Int) -> (Command, Int) -> (Int, Int)
calculatePosition (pos, depth) (cmd, value) = case cmd of
    Forward -> (pos + value, depth)
    Down    -> (pos, depth + value)
    Up      -> (pos, depth - value)


finalPositionFixed :: [(Command, Int)] -> Int
finalPositionFixed commands = let
    (pos, depth, aim) = foldl calculatePositionFixed (0, 0, 0) commands
    in pos * depth

calculatePositionFixed :: (Int, Int, Int) -> (Command, Int) -> (Int, Int, Int)
calculatePositionFixed (pos, depth, aim) (cmd, value) = case cmd of
    Forward -> (pos + value, depth + aim * value, aim)
    Down    -> (pos, depth, aim + value)
    Up      -> (pos, depth, aim - value)
