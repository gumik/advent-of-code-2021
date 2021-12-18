module Day18 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Char
import Control.Monad.State
import Control.Monad.Loops

data Fish = Number Int | Pair Fish Fish
type ParseState = State String

solution = Solution "day18" "" run

run _ = (NoSolution, NoSolution)

parseFish :: String -> ParseState Fish
parseFish = do
    char <- head <$> get
    case char of
        '[' -> parsePair
        _   -> parseNumber

parsePair :: String -> ParseState Fish
parsePair = do
    readChar
    sub1 <- parseFish
    readChar
    sub2 <- parseFish
    readChar
    return $ Pair sub1 sub2

parseNumber :: String -> ParseState Fish
parseNumber = do
    numStr <- untilM readChar $ gets isDigit
    return $ Number $ readNum numStr

readChar :: ParseState Char
readChar = state . map (head . fst) . splitAt 1