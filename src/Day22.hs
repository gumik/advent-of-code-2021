{-# LANGUAGE LambdaCase #-}
module Day22 where

import Common (Solution(Solution), NoSolution(..), readNum, toTuple, toTriple)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import qualified Data.Set as S

data Step = Step Operation ((Int, Int), (Int, Int), (Int, Int)) deriving Show
data Operation = On | Off deriving Show

solution :: Solution Int Int
solution = Solution "day22" "Reactor Reboot" run

run :: [Char] -> (Int, Int)
run input = let
    commands = parse input
    in (cubesOn commands, cubesOn' commands)

parse :: String -> [Step]
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

cubesOn :: [Step] -> Int
cubesOn = sum . map area . simulateBetterWay . filter (\(Step _ ((x1, x2), (y1, y2), (z1, z2))) -> all inSmallRange [x1, x2, y1, y2, z1, z2])

inSmallRange :: Int -> Bool
inSmallRange x = x >= -50 && x <= 50

simulateBetterWay :: [Step] -> [Segment]
simulateBetterWay = foldl step [] where
    step segments (Step operation ((minX, maxX), (minY, maxY), (minZ, maxZ))) = let
        segment = Segment minX (maxX+1) [Segment minY (maxY+1) [Segment minZ (maxZ+1) [Unit]]]
        in case operation of
            On -> segments `addS` segment
            Off -> segments `removeS` segment

cubesOn' :: [Step] -> Int
cubesOn' = sum . map area . simulateBetterWay

data Segment = Segment Int Int [Segment] | Unit deriving (Show, Eq)


add :: [Segment] -> Segment -> [Segment]
add segments Unit = segments
add [] segment = [segment]
add segments a@(Segment a1 a2 [subA]) = merge partA partB where
    partB = splitSegments segments (indices [a])
    partA = splitSegments [a] (indices segments)
add segments segment = error $ "unhandled case in add: " ++ show segments ++ ", " ++ show segment


splitSegments :: [Segment] -> [Int] -> [Segment]
splitSegments [] _ = []
splitSegments segs [] = segs
splitSegments segs@(seg@(Segment a1 a2 sub) : rest) idxs@(x:xs)
    | x <= a1           = splitSegments segs xs
    | x > a1 && x < a2  = Segment a1 x sub : splitSegments (Segment x a2 sub : rest) xs
    | x >= a2           = seg : splitSegments rest idxs

merge :: [Segment] -> [Segment] -> [Segment]
merge [] segments = segments
merge segments [] = segments
merge segsA@(a@(Segment a1 a2 [subA]):restA) segsB@(b@(Segment b1 b2 subB):restB)
    | a1 == b1 && a2 == b2  = (Segment a1 b2 (add subB subA)) : merge restA restB
    | a1 < b1               = a : merge restA segsB
    | b1 < a1               = b : merge segsA restB

indices :: [Segment] -> [Int]
indices = uniq . concat . map segIndices

uniq (a:b:xs) = if a == b then a:uniq xs else a:b:uniq xs
uniq xs = xs

segIndices (Segment a b _) = [a, b]


remove :: [Segment] -> Segment -> [Segment]
remove [Unit] Unit = []
remove segments Unit = segments
remove [] segment = []
remove (b@(Segment b1 b2 subB):rest) a@(Segment a1 a2 [subA])
    | a2 <= b1                       = b:rest
    | a2 < b2 && a1 < b1             = Segment b1 a2 (remove subB subA) : Segment a2 b2 subB : rest
    | a2 < b2 && a1 == b1            = Segment a1 a2 (remove subB subA) : Segment a2 b2 subB : rest
    | a2 < b2 && a1 > b1             = Segment b1 a1 subB : Segment a1 a2 (remove subB subA) : Segment a2 b2 subB : rest
    | a2 == b2 && a1 < b1            = Segment b1 b2 (remove subB subA) : rest
    | a2 == b2 && a1 == b1           = Segment b1 b2 (remove subB subA) : rest
    | a2 == b2 && a1 > b1            = Segment b1 a1 subB : Segment a1 a2 (remove subB subA) : rest
    | a2 > b2 && a1 < b1             = Segment b1 b2 (remove subB subA) : remove rest (Segment b2 a2 [subA])
    | a2 > b2 && a1 == b1            = Segment b1 b2 (remove subB subA) : remove rest (Segment b2 a2 [subA])
    | a2 > b2 && a1 > b1 && a1 < b2  = Segment b1 a1 subB : Segment a1 b2 (remove subB subA) : remove rest (Segment b2 a2 [subA])
    | otherwise                      = b : remove rest a
remove _ _ = error "unhandled case in remove"

simplify :: [Segment] -> [Segment]
simplify [] = []
simplify [Unit] = [Unit]
simplify (Segment a b [] : rest) = simplify rest
simplify [Segment a b sub] = [Segment a b (simplify sub)]
simplify (a@(Segment a1 a2 subA) : b@(Segment b1 b2 subB) : rest) =
    if a2 == b1 && subA' == subB'
        then simplify (Segment a1 b2 subA' : rest)
        else Segment a1 a2 subA' : simplify (Segment b1 b2 subB' : rest)
    where
        subA' = simplify subA
        subB' = simplify subB
simplify _ = error "unhaldled case in simplify"

addS :: [Segment] -> Segment -> [Segment]
addS s s1 = simplify $ s `add` s1
removeS :: [Segment] -> Segment -> [Segment]
removeS s s1 = simplify $ s `remove` s1

area :: Segment -> Int
area Unit = 1
area (Segment a b s) = (b - a) * sum (map area s)
