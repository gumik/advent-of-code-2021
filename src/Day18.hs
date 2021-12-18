module Day18 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad.State
import Control.Monad.Loops
import Debug.Trace

data Fish = Number Int | Pair Fish Fish deriving Eq
type ParseState = State String

instance Show Fish where
    show (Number x) = show x
    show (Pair l r) = "[" ++ show l ++ "," ++ show r ++ "]"

solution = Solution "day18" "" run

run input = let
    fishes = map (evalState parseFish) (lines input)
    in (magnitudeOfSum fishes, NoSolution)

parseFish :: ParseState Fish
parseFish = do
    char <- head <$> get
    case char of
        '[' -> parsePair
        _   -> parseNumber

parsePair :: ParseState Fish
parsePair = do
    readChar
    sub1 <- parseFish
    readChar
    sub2 <- parseFish
    readChar
    return $ Pair sub1 sub2

parseNumber :: ParseState Fish
parseNumber = do
    numStr <- whileM (gets (isDigit . head)) readChar
    return $ Number $ readNum numStr

readChar :: ParseState Char
readChar = state $ fromJust . uncons

split :: Fish -> Maybe Fish
split (Number x) = if x > 9
    then Just $ Pair (Number $ x `div` 2) (Number $ (x+1) `div` 2)
    else Nothing
split (Pair l r) = case (split l, split r) of
        (Just sl, _) -> Just $ Pair sl r
        (_, Just sr) -> Just $ Pair l sr
        _            -> Nothing

explode :: Fish -> Maybe Fish
explode fish = case explode' 4 fish of
    Just (fish, _, _) -> Just fish
    _ -> Nothing
    
explode' :: Int -> Fish -> Maybe (Fish, Int, Int)
explode' 0 fish@(Pair (Number l) (Number r)) = Just (Number 0, l, r)
explode' n (Pair l r) = case (explode' (n-1) l, explode' (n-1) r) of
    (Just (ll, vl, vr), _) -> Just (Pair ll (addLeft vr r), vl, 0)
    (_, Just (rr, vl, vr)) -> Just (Pair (addRight vl l) rr, 0, vr)
    _            -> Nothing
explode' _ _ = Nothing

addLeft :: Int -> Fish -> Fish
addLeft v (Number x) = Number (x+v)
addLeft v (Pair l r) = Pair (addLeft v l) r

addRight :: Int -> Fish -> Fish
addRight v (Number x) = Number (x+v)
addRight v (Pair l r) = Pair l (addRight v r)

reduce :: Fish -> Fish
reduce fish = case (explode fish, split fish) of
    (Just exploded, _) -> reduce exploded
    (_, Just splitted) -> reduce splitted
    _                  -> fish

addReduce :: Fish -> Fish -> Fish
addReduce  l = reduce . Pair l

magnitudeOfSum :: [Fish] -> Int
magnitudeOfSum = magnitude . foldl1 addReduce

magnitude :: Fish -> Int
magnitude (Number x) = x
magnitude (Pair l r) = 3 * magnitude l + 2 * magnitude r

maxSumOfPairs :: [Fish] -> Int
maxSumOfPairs fishes = maximum $ map magnitudeOfSum pairs where 
    pairs = [[f1, f2] | f1 <- fishes, f2 <- fishes, f1 /= f2]
    