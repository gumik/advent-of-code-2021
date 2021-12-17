{-# LANGUAGE LambdaCase #-}
module Day16 ( solution ) where

import Common (Solution(Solution), toDecimal)
import qualified Data.Map.Strict as M
import Control.Monad.State (State(..), replicateM, evalState, gets, state)
import Control.Monad.Loops (whileM, untilM)

data Packet = Packet {
    _version :: Int,
    _data :: PacketData
} deriving Show

data PacketData = Literal Int | Operator Int [Packet] deriving Show
data Bit = B0 | B1 deriving (Show, Eq)
type ParseState = State [Bit]

solution :: Solution Int Int
solution = Solution "day16" "Packet Decoder" run

run :: String -> (Int, Int)
run input = let
    packet = parse input
    in (sumOfVersions packet, evaluate packet)

hexToBin :: M.Map Char [Bit]
hexToBin = M.fromList [
    ('0', [B0, B0, B0, B0]),
    ('1', [B0, B0, B0, B1]),
    ('2', [B0, B0, B1, B0]),
    ('3', [B0, B0, B1, B1]),
    ('4', [B0, B1, B0, B0]),
    ('5', [B0, B1, B0, B1]),
    ('6', [B0, B1, B1, B0]),
    ('7', [B0, B1, B1, B1]),
    ('8', [B1, B0, B0, B0]),
    ('9', [B1, B0, B0, B1]),
    ('A', [B1, B0, B1, B0]),
    ('B', [B1, B0, B1, B1]),
    ('C', [B1, B1, B0, B0]),
    ('D', [B1, B1, B0, B1]),
    ('E', [B1, B1, B1, B0]),
    ('F', [B1, B1, B1, B1])]

parse :: String -> Packet
parse = evalState parsePacket . concatMap (hexToBin M.!) . filter (/= '\n')

parsePacket :: ParseState Packet
parsePacket = do
    version <- toDecimal' <$> getBits 3
    packetId <- toDecimal' <$> getBits 3
    packetData <- parsePacketData packetId
    return $ Packet version packetData

parsePacketData :: Int -> ParseState PacketData
parsePacketData packetId = do
    case packetId of
        4 -> parseLiteral
        _ -> do
            operatorType <- head <$> getBits 1
            case operatorType of
                B0 -> parseOperatorType0 packetId
                B1 -> parseOperatorType1 packetId

parseLiteral :: ParseState PacketData
parseLiteral = do
    segs <- whileM (gets $ (== B1) . head) (getBits 5)
    lastSeg <- getBits 5
    return $ Literal $ toDecimal' $ concatMap tail $ segs ++ [lastSeg]

parseOperatorType0 :: Int -> ParseState PacketData
parseOperatorType0 packetId = do
    totalLength <- toDecimal' <$> getBits 15
    ss <- getBits totalLength
    let subPackets = evalState (untilM parsePacket $ gets null) ss
    return $ Operator packetId subPackets

parseOperatorType1 :: Int -> ParseState PacketData
parseOperatorType1 packetId = do
    packetsNum <- toDecimal' <$> getBits 11
    subPackets <- replicateM packetsNum parsePacket
    return $ Operator packetId subPackets

getBits :: Int -> ParseState [Bit]
getBits = state . splitAt

toDecimal' :: [Bit] -> Int
toDecimal' = toDecimal 2 . map (\case B0 -> 0; B1 -> 1)

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
