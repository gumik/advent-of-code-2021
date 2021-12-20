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
import Debug.Trace (traceShow)
import Data.List (nub)

type Point = (Int, Int, Int)
type Offset = Point
type Scanner = [Point]
type Rotation = Scanner -> Scanner
type RotOff = Scanner -> Scanner

minimumCommon = 12

solution = Solution "day19" "" run

run input = let
    scanners = parse input
    -- m0 = S.fromList $ head scanners
    -- m1 = match m0 (scanners !! 1)
    -- m2 = match (fromJust m1) (scanners !! 4)
    -- m3 = match (fromJust m2) (scanners !! 3)
    -- m4 = match (fromJust m3) (scanners !! 2)
    matches = map (\(idx, (idx2, m)) -> (idx, (idx2, fromJust m))) $ filter (isJust . snd . snd) [(s1Idx, (s2Idx, match s1 s2)) | (s1Idx, s1) <- [0..] `zip` scanners, (s2Idx, s2) <- [0..] `zip` scanners, s1 /= s2] :: [(Int, (Int, (Rotation, Offset)))]
    matches' = M.fromListWith (++) (map (\(s1idx, (s2idx, (rotation, offset))) -> (s1idx, [(s2idx, rotation, offset)])) matches) :: M.Map Int [(Int, Rotation, Offset)]

    iterations = iterate merge (Iteration scanners matches' [0] S.empty (M.fromList [(idx, id) | idx <- [0..length scanners - 1]]))
    in traceShow (matchesDebug matches') (S.size $ _resultSet $ head $ dropWhile (not . null . _scannersToAdd) iterations, 0)

showIteration (Iteration _ matches toAdd resultSet rotOffs) = (matchesDebug matches, toAdd, S.size resultSet)

matchesDebug = M.map (map (\(idx, _, _) -> idx))

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

move :: Scanner -> Offset -> Scanner
move scanner (offX, offY, offZ) = map (\(x, y, z) -> (x+offX, y+offY, z+offZ)) scanner

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
    in if maxOverlapSize >= minimumCommon then Just (rotation, offset) else Nothing

maxCommonPoints :: S.Set Point -> [Point] -> (Int, Point)
maxCommonPoints p1 p2 = let
    offsets = S.toList $ S.fromList [(x1 - x2, y1 - y2, z1 - z2) | (x1, y1, z1) <- S.toList p1, (x2, y2, z2) <- p2] :: [Offset]
    p2offs = map (move p2) offsets :: [[Point]]
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

type Matches = M.Map Int [(Int, Rotation, Offset)]

data Iteration = Iteration {
    _scanners :: [Scanner],
    _matches :: Matches,
    _scannersToAdd :: [Int],
    _resultSet :: S.Set Point,
    _rotOffs :: M.Map Int RotOff}

merge :: Iteration -> Iteration
merge it@(Iteration _ _ [] _ _) = it
merge (Iteration scanners matches scannersToAdd resultSet rotOffs) = let
    (scanners', matches', resultSet', scannersToAdd', rotOffs') = foldl mergeScanner (scanners, matches, resultSet, [], rotOffs) scannersToAdd
    in traceShow (length scannersToAdd) $ Iteration scanners' matches' scannersToAdd' resultSet' rotOffs'

rotOff :: Rotation -> Offset -> Scanner -> Scanner
rotOff rotation offset scanner = move (rotation scanner) offset

mergeScanner :: ([Scanner], Matches, S.Set Point, [Int], M.Map Int RotOff) -> Int -> ([Scanner], Matches, S.Set Point, [Int], M.Map Int RotOff)
mergeScanner (scanners, matches, resultSet, pointsToAdd, rotOffs) s1Idx = let
    s1RotOff = rotOffs M.! s1Idx
    s1 = s1RotOff (scanners !! s1Idx)
    -- resultSet' = foldl S.union resultSet (map (S.fromList . snd) neighbours)
    resultSet' = S.union resultSet (S.fromList s1)
    neighbours = map (\(s2Idx, rotation, offset) -> (s2Idx, rotOff rotation offset)) $ matches M.! s1Idx :: [(Int, RotOff)]
    neighboursMap = M.fromList $ map (\(idx, rotOff) -> (idx, (rotOffs' M.! idx) (scanners !! idx))) neighbours
    --scanners' = zipWith (\idx scanner -> M.findWithDefault scanner idx neighboursMap) [0..] scanners
    scanners' = scanners
    matches' = M.map (filter (\(idx, _, _) -> idx /= s1Idx)) $ M.delete s1Idx matches
    pointsToAdd' = map fst neighbours
    rotOffs' = foldl (\rotOffs (idx, ro) -> M.update (\ro' -> Just $ s1RotOff . ro) idx rotOffs) rotOffs neighbours
    in (scanners', matches', resultSet', nub $ pointsToAdd ++ pointsToAdd', rotOffs')
