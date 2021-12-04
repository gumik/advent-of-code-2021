module Day04 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn, split)
import Data.Bifunctor (bimap)
import Data.List (transpose, partition)

solution :: Solution Int Int
solution = Solution "day04" "Giant Squid" run

run :: String -> (Int, Int)
run input = let
    (numbers, boards) = parse input
    steps = scanl step (Iteration boards 0 []) numbers
    in (scoreOfFirstWinningBoard steps, scoreOfLastWinningBoard steps)

type Board = [[Int]]


------ parsing

parse :: String -> ([Int], [Board])
parse input = let
    parts = splitOn "\n\n" input
    numbers = parseNumbers $ head parts
    boards = parseBoards $ tail parts
    in (numbers, boards)

parseBoards :: [String] -> [Board]
parseBoards = map parseBoard where
    parseBoard = filter (not . null) . map (map read . words) . splitOn "\n"

parseNumbers :: String -> [Int]
parseNumbers = map readNum . splitOn ","


------ solution

data Iteration = Iteration {
    iterationBoards :: [Board],
    iterationNumber :: Int,
    iterationWinningBoards :: [Board] }
    deriving Show

scoreOfFirstWinningBoard :: [Iteration] -> Int
scoreOfFirstWinningBoard steps = let
    Iteration _ number winningBoards = head $ dropWhile (null . iterationWinningBoards) steps
    winningBoard = head winningBoards
    in boardScore number winningBoard

scoreOfLastWinningBoard :: [Iteration] -> Int
scoreOfLastWinningBoard steps = let
    (boards, number) = bimap (iterationBoards . last) (iterationNumber . head) (break (null . iterationBoards) steps)
    board = markBoard number $ head boards
    in boardScore number board

step :: Iteration -> Int -> Iteration
step (Iteration boards _ wonBoards) number = let
    newBoards = markBoards number boards
    (winningBoards, remainingBoards) = partition isWinning newBoards
    in Iteration remainingBoards number (winningBoards ++ wonBoards) 

markBoards :: Int -> [Board] -> [Board]
markBoards number = map $ markBoard number

markBoard :: Int -> Board -> Board
markBoard number = map markInRow where
    markInRow row = map markNumber row
    markNumber numberInBoard = if numberInBoard == number then -1 else numberInBoard

isWinning :: Board -> Bool
isWinning board = let
    isWinningRow = any (all (== (-1)))
    isWinningByRow = isWinningRow board
    isWinningByCol = isWinningRow (transpose board)
    in isWinningByRow || isWinningByCol

boardScore :: Int -> Board -> Int
boardScore number board = let
    boardSum = sum $ concatMap (map (max 0)) board
    in boardSum * number


------ debug

-- showBoards boards = concat $ map showBoard boards

-- showBoard board = (concat $ map showRow board) ++ "\n" where
--     showRow row = show row ++ "\n"