module Day10 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap)
import Data.Maybe
import Data.List (sort, partition)

solution = Solution "day10" "Syntax Scoring" run

run input = let
    navigationCommands = parse input
    parsedCommands = map parseCommand navigationCommands
    (completions, invalidCommands) = partition (null . fst) parsedCommands
    in (totalSyntaxErrorScore invalidCommands, middleCompletionScore completions)

parse :: String -> [String]
parse = lines

type ParseResult = (String, String)

parseCommand :: String -> ParseResult
parseCommand chunks = parseCommand' chunks [] where
    parseCommand' [] stack = ([], stack)
    parseCommand' (x:xs) stack
      | isOpening x                     = parseCommand' xs (x:stack)
      | isComplementary (head stack) x  = parseCommand' xs (tail stack)
      | otherwise                       = (x:xs, stack)

isOpening :: Char -> Bool
isOpening c = c `elem` "([{<"

isComplementary :: Char -> Char -> Bool
isComplementary c1 c2 = c2 == (dropWhile (/= c1) parens !! 1) where
    parens = "()[]{}<>"


totalSyntaxErrorScore :: [ParseResult] -> Int
totalSyntaxErrorScore = sum . map (wrongParenScore . head . fst)

wrongParenScore :: Char -> Int
wrongParenScore c = case c of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _   -> error "wrong paren"


middleCompletionScore :: [ParseResult] -> Int
middleCompletionScore results = let
    completions = map snd results
    completionScores = sort $ map completionScore completions
    midIdx = length completionScores `div` 2
    in completionScores !! midIdx

completionScore :: String -> Int
completionScore completion = foldl (\total current -> 5*total + current) 0 $ map completionParenScore completion

completionParenScore :: Char -> Int
completionParenScore c = case c of
    '(' -> 1
    '[' -> 2
    '{' -> 3
    '<' -> 4
    _   -> error "wrong paren"
