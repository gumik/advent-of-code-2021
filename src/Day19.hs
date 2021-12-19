module Day19 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

type Point = (Int, Int, Int)
type Scanner = [Point]

solution = Solution "day19" "" run

run input = let
    scanners = parse input
    in (scanners, NoSolution)

parse :: String -> [Scanner]
parse = map parseScanner . splitOn "\n\n" where
    parseScanner = map parseLine . tail . lines
    parseLine = toTuple . map readNum . splitOn ","
    toTuple [x,y,z] = (x,y,z)

rotX :: Point -> Point
rotX (x, y, z) = (x, -z, y)

rotY :: Point -> Point
rotY (x, y, z) = (z, y, -x)

rotZ :: Point -> Point
rotZ (x, y, z) = (y, -x, z)

rotateX :: Scanner -> Scanner
rotateX = map rotX

rotateY :: Scanner -> Scanner
rotateY = map rotY

rotateZ :: Scanner -> Scanner
rotateZ = map rotZ

--  . . x | . . .
--  . . . | . x .
--  - - - - - - - -    (-1, 2) -> (2, 1) -> (1, -2) -> (-2, -1)
--  . x . | . . .       (x, y)  -> (y, -x)
--  . . . | x . .