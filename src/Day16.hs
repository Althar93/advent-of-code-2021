module Day16 (day16Solver) where

import Common
import Parser
import Data.Char
import Data.Maybe
import Data.Either

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day16_input.txt"

-- A simple digit parser
bitsLineParser :: String -> (Maybe String)
bitsLineParser s = Just $ s 

-- The types of the packets
data PacketTypeID = Literal | Sum | Product | Minimum | Maximum | GreaterThan | LessThan | Equal deriving Show

-- A single packet
data Packet = Packet {
    packetVersion   :: !Int,
    packetTypeID    :: !PacketTypeID,
    packetContents  :: !(Either Int [Packet])
} deriving Show

-- Converts an int to the corresponding packet type id
intToPacketTypeID :: Int -> PacketTypeID
intToPacketTypeID 0 = Sum
intToPacketTypeID 1 = Product
intToPacketTypeID 2 = Minimum
intToPacketTypeID 3 = Maximum
intToPacketTypeID 4 = Literal
intToPacketTypeID 5 = GreaterThan
intToPacketTypeID 6 = LessThan
intToPacketTypeID 7 = Equal 
intToPacketTypeID _ = error "Packet type ID not recognised"

-- Converts a hexadecimal string into its binary string counter part
hexStringToBinString :: String -> String
hexStringToBinString [] = []
hexStringToBinString (x:xs) = (hexCharToBinString x) ++ (hexStringToBinString xs) where
    hexCharToBinString '0' = "0000"
    hexCharToBinString '1' = "0001"
    hexCharToBinString '2' = "0010"
    hexCharToBinString '3' = "0011"
    hexCharToBinString '4' = "0100"
    hexCharToBinString '5' = "0101"
    hexCharToBinString '6' = "0110"    
    hexCharToBinString '7' = "0111"
    hexCharToBinString '8' = "1000"
    hexCharToBinString '9' = "1001"
    hexCharToBinString 'A' = "1010"
    hexCharToBinString 'B' = "1011"
    hexCharToBinString 'C' = "1100"
    hexCharToBinString 'D' = "1101"
    hexCharToBinString 'E' = "1110"
    hexCharToBinString 'F' = "1111"
    hexCharToBinString _   = error "Invalid hexadecimal character"

-- Converts a binary string to the corresponding int value
binStringToInt :: String -> Int
binStringToInt s = binStringToInt' 0 (reverse s) where
    binStringToInt' _ []    = 0
    binStringToInt' n (c:s) = (if (c == '1') then 2^n else 0) + (binStringToInt' (n + 1) s)

-- Parses an integer with the specified number of bits
parseBinInt :: Int -> Parser Int
parseBinInt 0 = error "Needs to specify >0 bits"
parseBinInt n = Parser $ \s -> case s of 
    [] -> []
    s' -> [(binStringToInt (take n s'), drop n s')]

-- Parses a single bit
parseBit :: Parser Bool
parseBit = Parser $ \s -> case s of
    []      -> []
    (c:s')  -> [(if c == '1' then True else False, s')]

-- Parses a nibble
parseNibble :: Parser String
parseNibble = Parser $ \s -> case s of
    [] -> []
    s' -> [(take 4 s', drop 4 s')]

-- Parses a group of nibbles
parseNibbleGroup :: Parser String
parseNibbleGroup = do
    isNotLastGroup <- parseBit
    nibble         <- parseNibble
    if isNotLastGroup 
        then fmap (nibble++) parseNibbleGroup
        else return nibble

-- Parses a literal
parseLiteral :: Parser Int
parseLiteral = do
    nibbleGroup <- parseNibbleGroup
    return $ binStringToInt nibbleGroup

-- Parses a packet
parsePacket :: Parser Packet
parsePacket = do
    pVersion  <- parseBinInt 3
    pTypeID   <- parseBinInt 3
    pContents <- pSelect (pTypeID == 4) parseLiteral parseSubPackets
    return Packet { packetVersion = pVersion, packetTypeID = (intToPacketTypeID pTypeID), packetContents = pContents}

-- Parses the sub packets
parseSubPackets :: Parser [Packet]
parseSubPackets = do
    lengthTypeID <- parseBit
    eitherLength <- pSelect (lengthTypeID == False) (parseBinInt 15) (parseBinInt 11)
    case eitherLength of
        Left totalLengthInBits -> do
            bits <- pRepeat totalLengthInBits parseItem 
            return $ runParser (pIterate parsePacket) bits
        Right numberOfSubPackets -> do 
            pRepeat numberOfSubPackets parsePacket

-- Evaluates the given packet
evaluatePacket :: Packet -> Int
evaluatePacket p = case packetTypeID p of
    Literal     -> fromLeft 0 (packetContents p) 
    Sum         -> sum      $ ps'
    Product     -> product  $ ps'
    Minimum     -> minimum  $ ps'
    Maximum     -> maximum  $ ps'
    GreaterThan -> if (ps' !! 0) >  (ps' !! 1) then 1 else 0
    LessThan    -> if (ps' !! 0) <  (ps' !! 1) then 1 else 0
    Equal       -> if (ps' !! 0) == (ps' !! 1) then 1 else 0
    where
        ps' = map evaluatePacket ps
        ps  = fromRight [] (packetContents p)

-- Reads the inputs from the designated input file
readInputs :: IO String
readInputs = do
    inputs <- readInputFile inputFilePath bitsLineParser
    return $ concat inputs

-- A smaller test input
testInput :: String
testInput = "D2FE28"

testInput2 :: String
testInput2 = "38006F45291200"

testInput3 :: String
testInput3 = "8A004A801A8002F478"

testInput4 :: String
testInput4 = "F600BC2D8F"

-- The actual solver for the day 16 puzzle (part 1)
solvePart1 :: String -> Int
solvePart1 xs = sumPacketVersion packet where
    packet = runParser parsePacket (hexStringToBinString xs)
    sumPacketVersion p = (packetVersion p) + case (packetContents p) of
        Right ps -> sum (map sumPacketVersion ps)
        _        -> 0

-- The actual solver for the day 16 puzzle (part 2)
solvePart2 :: String -> Int
solvePart2 xs = evaluatePacket packet where
    packet = runParser parsePacket (hexStringToBinString xs)

-- The day 16 puzzle
day16Solver :: IO [Int]
day16Solver = do
    input <- readInputs
    --let input = testInput4
    return [solvePart1 input, solvePart2 input]