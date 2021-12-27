module Day21 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe

data GameState = GameState {
    _round :: Int,
    _player1Score :: Int,
    _player2Score :: Int,
    _player1Pos :: Int,
    _player2Pos :: Int,
    _turn :: Turn
} deriving (Show, Eq, Ord)
data Turn = Player1Turn | Player2Turn deriving (Show, Eq, Ord)

solution = Solution "day21" "Dirac Dice" run

run _ = let
    p1Pos = 7
    p2Pos = 10
    dice = concat $ repeat [1..100]
    iterations = iterate game (GameState 0 0 0 p1Pos p2Pos Player1Turn, dice)
    in (part1 iterations, counts)

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

anyWin' :: GameState -> Bool
anyWin' (GameState _ p1Score p2Score _ _ _ ) = p1Score >= 21 || p2Score >= 21

f gs@(GameState round p1Score p2Score p1Pos p2Pos turn) states
    | anyWin' gs || gs `M.member` states  = M.alter add1 gs states
    | otherwise                           = M.insert gs 1 states
        
add1 :: Maybe Int -> Maybe Int
add1 Nothing = Just 1
add1 (Just x) = Just (x+1)

counts = M.toList $ M.fromListWith (+) [(a+b+c,1) | a<-[1..3], b<-[1..3], c<-[1..3]]
