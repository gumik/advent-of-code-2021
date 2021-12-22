{-# LANGUAGE LambdaCase #-}
module Day22 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, toTuple, toTriple)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import qualified Data.Set as S

data Step = Step Operation ((Int, Int), (Int, Int), (Int, Int)) deriving Show
data Operation = On | Off deriving Show

solution = Solution "day22" "" run

run input = let
    commands = parse input
    in (cubesOn commands, NoSolution)

parse = map parseLine . lines where
    parseLine l = let
        [opStr, rest] = splitOn " " l
        operation = parseOperation opStr
        coords = toTriple . map parseCoord $ splitOn "," rest
        in Step operation coords
    parseCoord = toTuple . map readNum . splitOn ".." . (!! 1) . splitOn "="
    parseOperation = \case
        "on" -> On
        "off" -> Off
        _ -> error "invalid operation"

simulateNaiveWay :: [Step] -> S.Set (Int, Int, Int)
simulateNaiveWay = foldl step S.empty where
    step pointSet (Step operation ((minX, maxX), (minY, maxY), (minZ, maxZ))) = let
        points = S.fromList [(x, y, z) | x <- [minX..maxX], y <- [minY..maxY], z <- [minZ..maxZ]]
        in case operation of
            On -> pointSet `S.union` points
            Off -> pointSet `S.difference` points

cubesOn :: [Step] -> Int
cubesOn = S.size . simulateNaiveWay . filter (\(Step _ ((x1, x2), (y1, y2), (z1, z2))) -> all inSmallRange [x1, x2, y1, y2, z1, z2])

inSmallRange :: Int -> Bool
inSmallRange x = x >= -50 && x <= 50
