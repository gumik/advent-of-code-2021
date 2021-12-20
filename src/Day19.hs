module Day19 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import qualified Data.Set as S
import Data.Maybe

type Point = (Int, Int, Int)
type Scanner = [Point]

solution = Solution "day19" "" run

run input = let
    scanners = parse input
    -- m0 = S.fromList $ head scanners
    -- m1 = match m0 (scanners !! 1)
    -- m2 = match (fromJust m1) (scanners !! 4)
    -- m3 = match (fromJust m2) (scanners !! 3)
    -- m4 = match (fromJust m3) (scanners !! 2)
    x = foldl (\(points, unmatched) scanner -> case match points scanner of Just m -> (m, unmatched); Nothing -> (points, scanner:unmatched)) (S.fromList $ head scanners, []) (tail scanners)
    in (S.size $ fst x, snd x)

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

rotations :: Scanner -> [Scanner]
rotations scanner = [angle $ side scanner | side <- sides, angle <- angles] where
    sides = [id, rotateX, rotateX . rotateX, rotateX . rotateX . rotateX, rotateZ, rotateZ . rotateZ . rotateZ]
    angles = [id, rotateY, rotateY . rotateY, rotateY . rotateY . rotateY]

match :: S.Set Point -> Scanner -> Maybe (S.Set Point)
match points scanner = let
    rot = rotations scanner
    commons = map (maxCommonPoints points) rot :: [(Int, S.Set Point)]
    commonsWithSize = map (\(commSize, comm) -> (commSize, comm)) commons :: [(Int, S.Set Point)]
    maxCommon = snd $ maximum commonsWithSize
    in if S.size maxCommon >= 12 then Just (S.union points maxCommon) else Nothing

maxCommonPoints :: S.Set Point -> [Point] -> (Int, S.Set Point)
maxCommonPoints p1 p2 = let
    offsets = S.toList $ S.fromList [(x1 - x2, y1 - y2, z1 - z2) | (x1, y1, z1) <- S.toList p1, (x2, y2, z2) <- p2]
    p2offs = map (\(offX, offY, offZ) -> map (\(x, y, z) -> (x+offX, y+offY, z+offZ)) p2) offsets :: [[Point]]
    in maximum $ map (\(common, p2off) -> (S.size common, S.fromList p2off)) $ map (\p2off -> (S.intersection p1 $ S.fromList p2off, p2off)) p2offs
