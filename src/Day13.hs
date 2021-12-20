{-# LANGUAGE TupleSections #-}
module Day13 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, showArray, ShowString(..))
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap, Bifunctor (second, first))
import Data.List (partition, nub, unfoldr, intercalate, transpose)
import Data.Array (array, elems)
import Debug.Trace (trace)

solution :: Solution Int ShowString
solution = Solution "day13" "Transparent Origami" run

run :: String -> (Int, ShowString)
run input = let
    (points, commands) = parse input
    folds = scanl fold points commands
    in (length (folds !! 1), showSheet $ last folds)

type Point = (Int, Int)
type Sheet = [Point]
type Command = (Char, Int)

parse :: String -> ([Point], [Command])
parse input = let
    [points, commands] = map lines $ splitOn "\n\n" input
    in (parsePoints points, parseCommands commands)

parsePoints :: [String] -> [Point]
parsePoints = map parseOne where
    parseOne line = let
        [x, y] = map readNum $ splitOn "," line
        in (x, y)

parseCommands :: [String] -> [Command]
parseCommands = map parseOne where
    parseOne line = let
        [axisCmd, value] = splitOn "=" line
        in (last axisCmd, readNum value)


fold :: Sheet -> Command -> Sheet
fold points (dir, foldAxis) = case dir of
    'x' -> fold' fst first points foldAxis
    'y' -> fold' snd second points foldAxis
    _   -> error "invalid command"

fold' :: (Point -> Int) -> ((Int -> Int) -> Point -> Point) -> [Point] -> Int -> [Point]
fold' axisSelector mapSelector points foldAxis = let
    (lower, higher) = partition ((< foldAxis) . axisSelector) points
    folded = map (mapSelector ((2*foldAxis)-)) higher
    in nub $ lower ++ folded

showSheet :: Sheet -> ShowString
showSheet sheet = let
    xs = map fst sheet
    ys = map snd sheet
    lowX = minimum xs
    highX = maximum xs
    lowY = minimum ys
    highY = maximum ys
    sheetArray = array ((lowX, lowY), (highX, highY)) $ [((x, y), ' ') | x <- [lowX..highX], y <- [lowY..highY]] ++ map (, '#') sheet
    in ShowString $ intercalate "\n" $ transpose $ unfoldr (takeMaybe (highY - lowY + 1)) $ elems sheetArray

takeMaybe :: Int -> [a] -> Maybe ([a], [a])
takeMaybe n l = case l of
    [] -> Nothing
    _  -> Just $ splitAt n l
