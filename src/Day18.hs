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
    numStr <- untilM readChar $ gets (isDigit . head)
    trace ("numStr '" ++ numStr ++ "'") $ return $ Number $ readNum numStr

readChar :: ParseState Char
readChar = state $ fromJust . uncons