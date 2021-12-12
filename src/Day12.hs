module Day12 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as M
import Data.Char (isUpper, isLower)
import Data.List (intercalate)
import Debug.Trace (trace)

solution :: Solution Int Int
solution = Solution "day12" "Passage Pathing" run

type Node = String
type Graph = M.Map Node [Node]
type Path = [Node]
type VisitedCounts = M.Map String Int
type CanVisitCheck = Graph -> VisitedCounts -> Node -> Bool

run :: String -> (Int, Int)
run input = let
    graph = parse input
    in (numberOfPaths graph canVisit, numberOfPaths graph canVisit')

parse :: String -> Graph
parse = M.fromListWith (++) . concatMap (edges . parseLine) . lines where
    parseLine = splitOn "-"

edges :: [Node] -> [(Node, [Node])]
edges [a, b] = case (a, b) of 
    ("start", b) -> [("start", [b])]
    (a, "start") -> [("start", [a])]
    (a, "end")   -> [(a, ["end"])]
    ("end", b)   -> [(b, ["end"])]
    (a, b)       -> [(a, [b]), (b, [a])]
edges _ = error "invalid input"

numberOfPaths :: Graph -> CanVisitCheck -> Int
numberOfPaths graph canVisit = let
    visitedCounts = M.map (const 0) graph
    in length $ paths canVisit graph visitedCounts "start"

paths :: CanVisitCheck -> Graph -> VisitedCounts -> Node -> [Path]
paths _ _ cnts "end" = [["end"]]
paths canVisit graph visitedCounts node = let
    nextNodes = graph M.! node
    visitedCounts' = M.adjust (+1) node visitedCounts
    furtherPaths = concatMap (paths canVisit graph visitedCounts') nextNodes
    in if canVisit graph visitedCounts' node
        then map (node:) furtherPaths
        else []

canVisit :: CanVisitCheck
canVisit graph visitedCounts node
    | isSmall node = visitedCounts M.! node == 1
    | otherwise    = True

canVisit' :: CanVisitCheck
canVisit' graph visitedCounts node
    | isSmall node  = visitedCounts M.! node <= 2  &&  M.size (M.filterWithKey (\k v -> isSmall k && v > 1) visitedCounts) < 2
    | otherwise     = True
    
isSmall :: Node -> Bool
isSmall = isLower . head
