module Day18 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.List
import Data.Char
import Control.Monad.State
import Control.Monad.Loops

data Fish = Number Int | Pair Fish Fish
type ParseState = State String

solution = Solution "day18" "" run

run _ = (NoSolution, NoSolution)

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
    return $ Number $ readNum numStr

readChar :: ParseState Char
readChar = state uncons