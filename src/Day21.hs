module Day21 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)

data GameState = GameState {
    _round :: Int,
    _player1Score :: Int,
    _player2Score :: Int,
    _player1Pos :: Int,
    _player2Pos :: Int,
    _dice :: [Int],
    _turn :: Turn
}
data Turn = Player1Turn | Player2Turn deriving Show

instance Show GameState where
    show (GameState round p1Score p2Score p1Pos p2Pos dice turn) =
        "GameState { round = " ++ show round ++
        ", p1Score = " ++ show p1Score ++
        ", p2Score = " ++ show p2Score ++
        ", p1Pos = " ++ show p1Pos ++
        ", p2Pos = " ++ show p2Pos ++
        ", dice = " ++ show (head dice) ++
        ", turn = " ++ show turn ++
        " }"

solution = Solution "day21" "Dirac Dice" run

run _ = let
    p1Pos = 7
    p2Pos = 10
    dice = concat $ repeat [1..100]
    iterations = iterate game (GameState 0 0 0 p1Pos p2Pos dice Player1Turn)
    in (part1 iterations, NoSolution)

game :: GameState -> GameState
game g@(GameState round p1Score p2Score p1Pos p2Pos (x1:x2:x3:xs) turn) = case turn of
    Player1Turn -> let pos = ((p1Pos + x1 + x2 + x3 - 1) `mod` 10) + 1 in GameState (round + 1) (p1Score + pos) p2Score pos p2Pos xs Player2Turn
    Player2Turn -> let pos = ((p2Pos + x1 + x2 + x3 - 1) `mod` 10) + 1 in GameState (round + 1) p1Score (p2Score + pos) p1Pos pos xs Player1Turn
game g = g

anyWin :: GameState -> Bool
anyWin (GameState _ p1Score p2Score _ _ _ _) = p1Score >= 1000 || p2Score >= 1000

part1 :: [GameState] -> Int
part1 iterations = let
    winningIteration = head $ dropWhile (not . anyWin) iterations
    GameState round p1Score p2Score _ _ _ _ = winningIteration
    in 3*round * min p1Score p2Score