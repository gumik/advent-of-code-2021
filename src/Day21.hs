module Day21 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

data GameState = GameState {
    _round :: Int,
    _player1Score :: Int,
    _player2Score :: Int,
    _player1Pos :: Int,
    _player2Pos :: Int,
    _turn :: Turn
} deriving (Show, Eq, Ord)
data Turn = Player1Turn | Player2Turn deriving Show

solution = Solution "day21" "Dirac Dice" run

run _ = let
    p1Pos = 7
    p2Pos = 10
    dice = concat $ repeat [1..100]
    iterations = iterate game (GameState 0 0 0 p1Pos p2Pos Player1Turn, dice)
    in (part1 iterations, NoSolution)

game :: (GameState, [Int]) -> (GameState, [Int])
game (g@(GameState round p1Score p2Score p1Pos p2Pos turn), x1:x2:x3:xs) = case turn of
    Player1Turn -> let pos = ((p1Pos + x1 + x2 + x3 - 1) `mod` 10) + 1 in (GameState (round + 1) (p1Score + pos) p2Score pos p2Pos Player2Turn, xs)
    Player2Turn -> let pos = ((p2Pos + x1 + x2 + x3 - 1) `mod` 10) + 1 in (GameState (round + 1) p1Score (p2Score + pos) p1Pos pos Player1Turn, xs)
game _ = error "unexpected arguments"

anyWin :: GameState -> Bool
anyWin (GameState _ p1Score p2Score _ _ _ ) = p1Score >= 1000 || p2Score >= 1000

part1 :: [(GameState, [Int])] -> Int
part1 iterations = let
    winningIteration = head $ dropWhile (not . anyWin) $ map fst iterations
    GameState round p1Score p2Score _ _ _ = winningIteration
    in 3*round * min p1Score p2Score

f gs@(GameState round p1Score p2Score p1Pos p2Pos turn) states =
    if gs `M.elem` states
        then M.adjust (+1) gs
        else gs `M.insert` states