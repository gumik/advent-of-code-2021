module Day21 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad.State.Strict

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
    in (part1 iterations, evalState (f (GameState 0 (PlayerStat 0 p1Pos) (PlayerStat 0 p2Pos) Player1Turn)) M.empty)

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

anyWin :: Int -> GameState -> Bool
anyWin score gs = p1Win score gs || p2Win score gs

p1Win :: Int -> GameState -> Bool
p1Win score (GameState _ (PlayerStat p1Score _) _ _ ) = p1Score >= score

p2Win :: Int -> GameState -> Bool
p2Win score (GameState _ _ (PlayerStat p2Score _) _ ) = p2Score >= score

part1 :: [(GameState, [Int])] -> Int
part1 iterations = let
    winningIteration = head $ dropWhile (not . anyWin 1000) $ map fst iterations
    GameState round (PlayerStat p1Score _) (PlayerStat p2Score _) _ = winningIteration
    in 3*round * min p1Score p2Score

type DiracState = State (M.Map GameState Int)

f :: GameState -> DiracState Int
f gs@(GameState round p1 p2 turn) = do
    states <- get
    if p1Win 21 gs then return 1
    else if p2Win 21 gs then return 0
    else if gs `M.member` states then return $ states M.! gs
    else do
        result <- foldM (g gs) 0 counts
        put $ M.insert gs result states
        return result

step :: GameState -> Int -> GameState
step (GameState _ p1 p2 turn) x = case turn of
    Player1Turn -> GameState 0 (move p1 x) p2 Player2Turn
    Player2Turn -> GameState 0 p1 (move p2 x) Player1Turn

g :: GameState -> Int -> (Int, Int) -> DiracState Int
g gs acc (x, cnt) = do
    rf <- f (step gs x)
    return $ acc + cnt * rf

counts = M.toList $ M.fromListWith (+) [(a+b+c,1) | a<-[1..3], b<-[1..3], c<-[1..3]]
