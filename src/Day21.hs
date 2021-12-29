module Day21 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe
import Control.Monad.State.Strict

data GameState = GameState {
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
    (part2, states) = runState (f (GameState (PlayerStat 0 p1Pos) (PlayerStat 0 p2Pos) Player1Turn)) M.empty
    in (part1 p1Pos p2Pos, (part2, M.size states))

game :: (GameState, [Int]) -> (GameState, [Int])
game (gs, dice) = (step gs (sum $ take 3 dice), drop 3 dice)

move :: PlayerStat -> Int -> PlayerStat
move (PlayerStat score pos) x = PlayerStat (score + pos') pos' where
    pos' = ((pos + x  - 1) `mod` 10) + 1

step :: GameState -> Int -> GameState
step (GameState p1 p2 turn) x = case turn of
    Player1Turn -> GameState (move p1 x) p2 Player2Turn
    Player2Turn -> GameState p1 (move p2 x) Player1Turn

anyWin :: Int -> GameState -> Bool
anyWin score gs = p1Win score gs || p2Win score gs

p1Win :: Int -> GameState -> Bool
p1Win score (GameState (PlayerStat p1Score _) _ _ ) = p1Score >= score

p2Win :: Int -> GameState -> Bool
p2Win score (GameState _ (PlayerStat p2Score _) _ ) = p2Score >= score

part1 :: Int -> Int -> Int
part1 p1Pos p2Pos = let
    dice = concat $ repeat [1..100]
    iterations = iterate game (GameState (PlayerStat 0 p1Pos) (PlayerStat 0 p2Pos) Player1Turn, dice)
    winningIteration = head $ dropWhile (not . anyWin 1000 . snd) $ zip [0..] $ map fst iterations
    (round, GameState (PlayerStat p1Score _) (PlayerStat p2Score _) _) = winningIteration
    in 3*round * min p1Score p2Score

type DiracState = State (M.Map GameState Int)

f :: GameState -> DiracState Int
f gs@(GameState p1 p2 turn) = do
    states <- get
    if p1Win 21 gs then return 1
    else if p2Win 21 gs then return 0
    else if gs `M.member` states then return $ states M.! gs
    else do
        result <- foldM (g gs) 0 counts
        modify $ M.insert gs result
        return result


g :: GameState -> Int -> (Int, Int) -> DiracState Int
g gs acc (x, cnt) = f (step gs x) >>= return $ acc + cnt *

counts = M.toList $ M.fromListWith (+) [(a+b+c,1) | a<-[1..3], b<-[1..3], c<-[1..3]]
