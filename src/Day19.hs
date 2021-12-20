module Day19 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap, second)
import qualified Data.Set as S
import Data.Maybe ( fromJust, isJust )
import Data.Foldable (maximumBy)
import Data.Function (on)
import qualified Data.Array as A
import qualified Data.Map as M
import Debug.Trace (traceShow, trace)
import Data.List (nub)

-- Really ugly code. But struggled so much with this puzzle that I don't like to make it better. :)

type Point = (Int, Int, Int)
type Offset = Point
type Scanner = [Point]
type Rotation = Scanner -> Scanner
type RotOff = Scanner -> Scanner

minimumCommon :: Int
minimumCommon = 12

solution :: Solution Int Int
solution = Solution "day19" "Beacon Scanner" run

run :: String -> (Int, Int)
run input = let
    scanners = parse input
    iterations = iterate merge (Iteration scanners (findMatches M.empty scanners 0) [0] S.empty (M.fromList [(idx, id) | idx <- [0..length scanners - 1]]))
    Iteration _ _ _ resultSet rotOffs = head $ dropWhile (not . null . _scannersToAdd) iterations
    scannersPositions = map (head . ($ [(0,0,0)])) $ M.elems rotOffs
    in (S.size resultSet, maxDistance scannersPositions)

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

rotations :: [Rotation]
rotations = [angle . side | side <- sides, angle <- angles] where
    sides = [id, rotateX, rotateX . rotateX, rotateX . rotateX . rotateX, rotateZ, rotateZ . rotateZ . rotateZ]
    angles = [id, rotateY, rotateY . rotateY, rotateY . rotateY . rotateY]

findMatches :: Matches -> [Scanner] -> Int -> Matches
findMatches matches scanners s1Idx = let
    s1 = scanners !! s1Idx
    m = map (\(idx, (idx2, m)) -> (idx, (idx2, fromJust m))) $ filter (isJust . snd . snd) [(s1Idx, (s2Idx, match s1 s2)) | (s2Idx, s2) <- [0..] `zip` scanners, s1 /= s2, not $ s2Idx `M.member` matches] :: [(Int, (Int, (Rotation, Offset)))]
    in M.union matches $ M.fromListWith (++) (map (\(s1idx, (s2idx, (rotation, offset))) -> (s1idx, [(s2idx, rotation, offset)])) m) :: M.Map Int [(Int, Rotation, Offset)]

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
    commons = map (second S.fromList . (\p2off -> (S.intersection p1 $ S.fromList p2off, p2off))) p2offs
    commons' = commons `zip` offsets
    in maximum $ map (\((common, p2off), offset) -> (S.size common, offset)) commons'

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
    in Iteration scanners' matches' scannersToAdd' resultSet' rotOffs'

rotOff :: Rotation -> Offset -> Scanner -> Scanner
rotOff rotation offset scanner = move (rotation scanner) offset

mergeScanner :: ([Scanner], Matches, S.Set Point, [Int], M.Map Int RotOff) -> Int -> ([Scanner], Matches, S.Set Point, [Int], M.Map Int RotOff)
mergeScanner (scanners, matches, resultSet, pointsToAdd, rotOffs) s1Idx = let
    s1RotOff = rotOffs M.! s1Idx
    s1 = s1RotOff (scanners !! s1Idx)
    resultSet' = S.union resultSet (S.fromList s1)
    neighbours = map (\(s2Idx, rotation, offset) -> (s2Idx, rotOff rotation offset)) $ matches M.! s1Idx :: [(Int, RotOff)]
    scanners' = scanners
    matches' = foldl(\matches point -> M.insertWith (++) point [] $ findMatches matches scanners point) matches pointsToAdd'
    pointsToAdd' = map fst neighbours
    rotOffs' = foldl (\rotOffs (idx, ro) -> M.insert idx (s1RotOff . ro) rotOffs) rotOffs neighbours  -- if scanners are rotated when added then this maybe is not needed?
    in (scanners', matches', resultSet', nub $ pointsToAdd ++ pointsToAdd', rotOffs')

distance :: Point -> Point -> Int
distance (x1, y1, z1) (x2, y2, z2) = abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

maxDistance :: [Point] -> Int
maxDistance points = maximum [distance p1 p2 | p1 <- points, p2 <- points]
