module Day16 ( solution ) where

import Common (Solution(Solution), NoSolution(..), readNum, toDecimal, readNum')
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Debug.Trace (trace, traceShow)
import Data.Maybe (listToMaybe)

data Packet = Packet {
    _version :: Int,
    _data :: PacketData
} deriving Show

data PacketData = Literal Int | Operator Int [Packet] deriving Show

solution :: Solution Int Int
solution = Solution "day16" "Packet Decoder" run

run :: String -> (Int, Int)
run input = let
    packet = parse input
    in (sumOfVersions packet, evaluate packet)

hexToBin :: M.Map Char [Char]
hexToBin = M.fromList [
    ('0', "0000"),
    ('1', "0001"),
    ('2', "0010"),
    ('3', "0011"),
    ('4', "0100"),
    ('5', "0101"),
    ('6', "0110"),
    ('7', "0111"),
    ('8', "1000"),
    ('9', "1001"),
    ('A', "1010"),
    ('B', "1011"),
    ('C', "1100"),
    ('D', "1101"),
    ('E', "1110"),
    ('F', "1111")]

parse :: String -> Packet
parse = fst . parsePacket . concatMap (hexToBin M.!) . filter (/= '\n')

parsePacket :: String -> (Packet, String)
parsePacket str = let
    (versionStr, str') = splitAt 3 str
    (packetIdStr, str'') = splitAt 3 str'
    version = toDecimal' versionStr
    packetId = toDecimal' packetIdStr
    (packetData, str''') = case packetId of
        4 -> parseLiteral str''
        _ -> parseOperator packetId str''
    in (Packet version packetData, str''')

parseLiteral :: String -> (PacketData, String)
parseLiteral str = let
    parts = segments str
    (firstSegments, rest) = span ((== '1') . head) parts
    binaryNum = concatMap tail firstSegments ++ tail (head rest)
    in (Literal (toDecimal' binaryNum), concat $ tail rest)

parseOperator :: Int -> String -> (PacketData, String)
parseOperator operatorId (lengthTypeID:rest) = case lengthTypeID of
    '0' -> let
        (totalLengthStr, rest') = splitAt 15 rest
        totalLength = toDecimal' totalLengthStr
        (subPackets, _) = parseSubPackets (take totalLength rest')
        in (Operator operatorId subPackets, drop totalLength rest')
    '1' -> let
        (packetsNumStr, rest') = splitAt 11 rest
        packetsNum = toDecimal' packetsNumStr
        (subPackets, rest'') = parseSubPacketsN packetsNum rest'
        in (Operator operatorId subPackets, rest'')
    _   -> error "invalid operator length type id"
parseOperator _ _ = error "data for operator too short"

parseSubPackets :: String -> ([Packet], String)
parseSubPackets [] = ([], [])
parseSubPackets str = let
    (packet, rest) = parsePacket str
    (packets, rest') = parseSubPackets rest
    in (packet : packets, rest')

parseSubPacketsN :: Int -> String -> ([Packet], String)
parseSubPacketsN 0 rest = ([], rest)
parseSubPacketsN n str = let
    (packet, rest) = parsePacket str
    (packets, rest') = parseSubPacketsN (n-1) rest
    in (packet : packets, rest')

toDecimal' :: String -> Int
toDecimal' = toDecimal 2 . map readNum'

segments :: [a] -> [[a]]
segments [] = []
segments str = a : segments b where
    (a, b) = splitAt 5 str


sumOfVersions :: Packet -> Int
sumOfVersions (Packet version packetData) = version + sumOfContained where
    sumOfContained = case packetData of
        Literal _ -> 0
        Operator _ subPackets -> sum $ map sumOfVersions subPackets

evaluate :: Packet -> Int
evaluate (Packet _ packetData) = case packetData of
    Literal x -> x
    Operator operatorId subPackets -> case operatorId of
        0 -> sum subPacketsValues
        1 -> product subPacketsValues
        2 -> minimum subPacketsValues
        3 -> maximum subPacketsValues
        5 -> let [v1, v2] = subPacketsValues in if v1 > v2 then 1 else 0
        6 -> let [v1, v2] = subPacketsValues in if v1 < v2 then 1 else 0
        7 -> let [v1, v2] = subPacketsValues in if v1 == v2 then 1 else 0
        _ -> error "invalid packet type"
      where
        subPacketsValues = map evaluate subPackets
