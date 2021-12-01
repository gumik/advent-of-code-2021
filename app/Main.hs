module Main where

import qualified Data.Map as Map
import System.Environment
import Day01
import Day02

main :: IO ()
main = do
    args <- getArgs
    let dayRun = Map.lookup (head args) daysMap
    maybe (putStrLn "invalid day specified") id dayRun

daysMap = Map.fromList 
    [("day01", Day01.run)
    ,("day02", Day02.run)]