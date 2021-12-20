module Day19 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import qualified Data.Set as S
import Data.Maybe
import Data.Foldable (maximumBy)
import Data.Function (on)
import qualified Data.Array as A
import qualified Data.Map as M

type Point = (Int, Int, Int)
type Offset = Point
type Scanner = [Point]
type Rotation = Scanner -> Scanner

solution = Solution "day19" "" run

run input = let
    scanners = parse input
    -- m0 = S.fromList $ head scanners
    -- m1 = match m0 (scanners !! 1)
    -- m2 = match (fromJust m1) (scanners !! 4)
    -- m3 = match (fromJust m2) (scanners !! 3)
    -- m4 = match (fromJust m3) (scanners !! 2)
    matches = filter (isJust . snd . snd) [(s1Idx, (s2Idx, match s1 s2)) | (s1Idx, s1) <- [0..] `zip` scanners, (s2Idx, s2) <- [0..] `zip` scanners, s1 /= s2]
    matches' = M.fromListWith (++) $ map (\(s1idx, (s2idx, _)) -> (s1idx, [s2idx])) matches
    in (matches', 0)

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

rotations :: [Rotation]
rotations = [angle . side | side <- sides, angle <- angles] where
    sides = [id, rotateX, rotateX . rotateX, rotateX . rotateX . rotateX, rotateZ, rotateZ . rotateZ . rotateZ]
    angles = [id, rotateY, rotateY . rotateY, rotateY . rotateY . rotateY]

match :: Scanner -> Scanner -> Maybe (Rotation, Offset)
match points scanner = let
    pointsSet = S.fromList points
    commons = map (\rotation -> (maxCommonPoints pointsSet (rotation scanner), rotation)) rotations :: [((Int, Point), Rotation)] -- ((overlap cnt, offset), rotation)
    ((maxOverlapSize, offset), rotation) = maximumBy (compare `on` fst) commons
    in if maxOverlapSize >= 12 then Just (rotation, offset) else Nothing

maxCommonPoints :: S.Set Point -> [Point] -> (Int, Point)
maxCommonPoints p1 p2 = let
    offsets = S.toList $ S.fromList [(x1 - x2, y1 - y2, z1 - z2) | (x1, y1, z1) <- S.toList p1, (x2, y2, z2) <- p2] :: [Offset]
    p2offs = map (\(offX, offY, offZ) -> map (\(x, y, z) -> (x+offX, y+offY, z+offZ)) p2) offsets :: [[Point]]
    commons = map (\(common, p2off) -> (common, S.fromList p2off)) $ map (\p2off -> (S.intersection p1 $ S.fromList p2off, p2off)) p2offs
    commons' = filter (\((common, p2off), offset) -> isCorrectOverlap p1 (common, p2off)) (commons `zip` offsets)
    in maximum $ map (\((common, p2off), offset) -> (S.size common, offset)) commons'

isCorrectOverlap :: S.Set Point -> (S.Set Point, S.Set Point) -> Bool
isCorrectOverlap p1 (common, p2) = let
    commonPoints = S.toList common
    minX = minimum $ map (\(x, _, _) -> x) commonPoints
    maxX = maximum $ map (\(x, _, _) -> x) commonPoints
    minY = minimum $ map (\(_, y, _) -> y) commonPoints
    maxY = maximum $ map (\(_, y, _) -> y) commonPoints
    minZ = minimum $ map (\(_, _, z) -> z) commonPoints
    maxZ = maximum $ map (\(_, _, z) -> z) commonPoints
    p1InCommonRange = filter (\(x, y, z) -> x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ) $ S.toList p1
    p2InCommonRange = filter (\(x, y, z) -> x >= minX && x <= maxX && y >= minY && y <= maxY && z >= minZ && z <= maxZ) $ S.toList p2
    in True --length p1InCommonRange == length commonPoints && length p2InCommonRange == length commonPoints
