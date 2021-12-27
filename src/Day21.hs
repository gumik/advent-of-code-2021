module Day21 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe

data GameState = GameState {
    _round :: Int,
    _player1Stat :: PlayerStat,
    _player2Stat :: PlayerStat,
    _turn :: Turn
} deriving (Show, Eq, Ord)
data Turn = Player1Turn | Player2Turn deriving (Show, Eq, Ord)
data PlayerStat = PlayerStat {
    _score :: Int,
    _pos :: Int
} deriving (Show, Eq, Ord)

solution = Solution "day21" "Dirac Dice" run

run _ = let
    p1Pos = 7
    p2Pos = 10
    dice = concat $ repeat [1..100]
    iterations = iterate game (GameState 0 (PlayerStat 0 p1Pos) (PlayerStat 0 p2Pos) Player1Turn, dice)
    in (part1 iterations, counts)

game :: (GameState, [Int]) -> (GameState, [Int])
game (g@(GameState round p1 p2 turn), x1:x2:x3:xs) = case turn of
    Player1Turn -> (GameState (round + 1) (move p1 x) p2 Player2Turn, xs)
    Player2Turn -> (GameState (round + 1) p1 (move p2 x) Player1Turn, xs)
  where
    x = x1+x2+x3
game _ = error "unexpected arguments"

move :: PlayerStat -> Int -> PlayerStat
move (PlayerStat score pos) x = PlayerStat (score + pos') pos' where
    pos' = ((pos + x  - 1) `mod` 10) + 1 

anyWin :: Int -> GameState -> Int -> Bool
anyWin score (GameState _ (PlayerStat p1Score _) (PlayerStat p2Score _) _ ) = p1Score >= score || p2Score >= score

part1 :: [(GameState, [Int])] -> Int
part1 iterations = let
    winningIteration = head $ dropWhile (not . anyWin 1000) $ map fst iterations
    GameState round (PlayerStat p1Score _) (PlayerStat p2Score _) _ = winningIteration
    in 3*round * min p1Score p2Score

f gs@(GameState round p1Score p2Score p1Pos p2Pos turn) states
    | anyWin 21 gs || gs `M.member` states  = M.alter add1 gs states
    | otherwise                           = M.insert gs 1 states
        
add1 :: Maybe Int -> Maybe Int
add1 Nothing = Just 1
add1 (Just x) = Just (x+1)

counts = M.toList $ M.fromListWith (+) [(a+b+c,1) | a<-[1..3], b<-[1..3], c<-[1..3]]
