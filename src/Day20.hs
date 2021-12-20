{-# LANGUAGE LambdaCase #-}
module Day20 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, toDecimal, ShowString(..), showArray)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import qualified Data.Set as S
import Data.Array (array, listArray, Array, bounds, (!), elems)
import Data.List (intercalate)
import Debug.Trace (traceShow, trace)

type Algo = Array Int Pixel
type Point = (Int, Int)
data Pixel = Light | Dark deriving Eq
type Pixels = Array (Int, Int) Pixel
data Floor = Floor {
    _outer :: Pixel,
    _pixels :: Pixels} deriving Show

instance Show Pixel where
    show Light = "#"
    show Dark = "."

solution :: Solution Int Int
solution = Solution "day20" "Trench Map" run

run :: String -> (Int, Int)
run input = let
    (algo, image) = parse input
    enhancements = iterate (enhance algo) image
    afterTwoEnhancements = enhancements !! 2
    afterFiftyEnhancements = enhancements !! 50
    in (numberOfLightPixels afterTwoEnhancements, numberOfLightPixels afterFiftyEnhancements)

parse :: String -> (Algo, Floor)
parse = (\[a, b] -> (parseAlgo a, Floor Dark $ parseImage b)) . splitOn "\n\n" where
    parseAlgo = listArray (0, 511) . map readPixel
    parseImage = parseArray readPixel

readPixel :: Char -> Pixel
readPixel = \case '#' -> Light; _ -> Dark

enhance :: Algo -> Floor -> Floor
enhance algo floor@(Floor outer pixels) = let
    ((minX, minY), (maxX, maxY)) = bounds pixels
    pointsToCheck = [(x, y) | x <- [minX-1 .. maxX+1], y <- [minY-1 .. maxY+1]]
    enhancedPoints = map (pixelEnhanced algo floor) pointsToCheck
    pixels' = array ((minX-1, minY-1), (maxX+1, maxY+1)) (pointsToCheck `zip` enhancedPoints)
    outer' = pixelEnhanced algo floor(minX-2, minY-2)
    in Floor outer' pixels'

pixelEnhanced :: Algo -> Floor -> Point -> Pixel
pixelEnhanced algo floor@(Floor outer lightPixels) (y,x) = let
    values = map (pixelValue floor) [(y-1, x-1), (y-1, x), (y-1, x+1), (y, x-1), (y, x), (y, x+1), (y+1, x-1), (y+1, x), (y+1, x+1)]
    idx = toDecimal 2 $ map (\case Light -> 1; _ -> 0) values
    in algo ! idx

pixelValue :: Floor -> Point -> Pixel
pixelValue (Floor outer pixels) (x, y) = if x >= minX && x <= maxX && y >= minY && y <= maxY then pixels ! (x,y) else outer where
    ((minX, minY), (maxX, maxY)) = bounds pixels

numberOfLightPixels :: Floor -> Int
numberOfLightPixels (Floor _ pixels) = length $ filter (== Light) $ elems pixels
