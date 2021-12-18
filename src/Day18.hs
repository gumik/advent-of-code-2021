module Day18 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.List
import Data.Char
import Data.Maybe
import Control.Monad.State
import Control.Monad.Loops
import Debug.Trace

data Fish = Number Int | Pair Fish Fish deriving Show
type ParseState = State String

solution = Solution "day18" "" run

run input = let
    fishes = map (evalState parseFish) (lines input)
    in (intercalate "\n" $ map show fishes, NoSolution)

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
    trace ("numStr '" ++ numStr ++ "'") $ return $ Number $ readNum numStr

readChar :: ParseState Char
readChar = state $ fromJust . uncons

split :: Fish -> Maybe Fish
split (Number x) = if x > 9
    then Just $ Pair (Number $ x `div` 2) (Number $ (x+1) `div` 2)
    else Nothing
split (Pair l r) = let
    sl = split l
    sr = split r
    in if isJust sl then Just $ Pair l r
        else if isJust sr then Just $ Pair l r
            else Nothing
    