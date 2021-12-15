module Day15 ( solution, parse, step, initialQueue, relax ) where

import Common (Solution(Solution), NoSolution(..), readNum, parseArray, readNum', inArrayBounds, showArray)
import Data.Array(Array, bounds, (!), listArray)
import qualified Data.PSQueue as Q
import Data.PSQueue (Binding ((:->)))
import GHC.Base (maxInt)
import Debug.Trace (trace, traceShow)
import qualified Data.Map.Strict as M
import Data.Maybe ( fromJust )

type Point = (Int, Int)
type PQueue = Q.PSQ Point Int
type CostArray = Array Point Int
type BestPathCost = M.Map Point Int

data Iteration = Iteration {
    iterationCostArray :: CostArray,
    iterationQueue :: PQueue,
    iterationBestPathCost :: BestPathCost } deriving (Show)

solution :: Solution Int Int
solution = Solution "day15" "" run

run :: String -> (Int, Int)
run input = let
    costArray = parse input
    costArray' = extend input 5
    in (bestPathCost costArray, bestPathCost costArray')

parse :: String -> CostArray
parse = parseArray readNum'

extend :: String -> Int -> CostArray
extend input n = let
    input' = lines input
    height = length input'
    width = length $ head input'
    extended = extendLine n $ concatMap (extendLine n . map readNum') input'
    in listArray ((0, 0), (n*height-1, n*width-1)) extended

extendLine :: Int -> [Int] -> [Int]
extendLine n = concat . take n . iterate (map (\x -> (x `mod` 9) + 1))

bestPathCost :: CostArray -> Int
bestPathCost costArray = let
    bestPathCost = initialBestPathCost costArray
    queue = initialQueue bestPathCost
    Iteration _ _ bestPathCost' = head $ dropWhile (not . Q.null . iterationQueue) $ iterate step (Iteration costArray queue bestPathCost)
    (_, destinationPoint) = bounds costArray
    in bestPathCost' M.! destinationPoint

initialQueue :: BestPathCost -> PQueue
initialQueue = Q.fromList . map (uncurry (:->)) . M.toList

initialBestPathCost :: CostArray -> BestPathCost
initialBestPathCost costArray = M.fromList $ ((0, 0), 0)  : [((y,  x), maxInt) | x <- [minX..maxX], y <- [minY..maxY], (y, x) /= (0, 0)] where
    ((minY, minX), (maxY, maxX)) = bounds costArray

step :: Iteration -> Iteration
step iteration@(Iteration costArray queue bestPathCost)
    | Q.null queue = iteration
    | otherwise = let
        (point :-> cost, queue') = fromJust $ Q.minView queue
        points = neighbours costArray point
        (queue'', bestPathCost') = foldl (relax costArray cost) (queue', bestPathCost) points
        in Iteration costArray queue'' bestPathCost'

neighbours :: CostArray -> Point -> [Point]
neighbours grid (y, x) = filter (inArrayBounds grid) [(y, x-1), (y-1, x), (y, x+1), (y+1, x)]

relax :: CostArray -> Int -> (PQueue, BestPathCost) -> Point -> (PQueue, BestPathCost)
relax costArray costFrom (queue, bestPathCost) pointTo = (queue', bestPathCost') where
    queue' = Q.alter (fmap (min newCost)) pointTo queue
    bestPathCost' = M.adjust (min newCost) pointTo bestPathCost
    newCost = costTo + costFrom
    costTo = costArray ! pointTo
