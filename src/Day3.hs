module Day3 (day3Solver) where

import Common

-- The bit field type
type BitField = [Int]

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day3_input.txt"

-- A basic line parser
bitfieldLineParser :: String -> (Maybe BitField)
bitfieldLineParser xs = Just $ map readBit xs where
    readBit '1'  = 1
    readBit _    = 0

-- The test input
testInput :: [BitField]
testInput = [
    [0, 0, 1, 0, 0],
    [1, 1, 1, 1, 0],
    [1, 0, 1, 1, 0],
    [1, 0, 1, 1, 1],
    [1, 0, 1, 0, 1],
    [0, 1, 1, 1, 1],
    [0, 0, 1, 1, 1],
    [1, 1, 1, 0, 0],
    [1, 0, 0, 0, 0],
    [1, 1, 0, 0, 1],
    [0, 0, 0, 1, 0],
    [0, 1, 0, 1, 0]
    ]

-- Convert a bit-field into the corresponding decimal number
toDecimal :: BitField -> Int
toDecimal xs = toDecimal' xs (length xs - 1) 0 where
    toDecimal' (x:xs) p n   = toDecimal' xs (p - 1) (n + (if x == 1 then 2^p else 0))
    toDecimal' [] _ n       = n

-- Sums the bits of a bit-field
bitFieldSum :: [BitField] -> [Int]
bitFieldSum xs = foldr bitFieldSum' (take (length (xs !! 0)) (repeat 0)) xs where
                bitFieldSum' as bs = zipWith (+) as bs

-- Returns the half length of an array
halfLength :: [a] -> Float
halfLength xs = fromIntegral (length xs) / fromIntegral 2

-- Compute the most significant bit of the specified bit-field
mostSignificantBitField :: [BitField] -> BitField
mostSignificantBitField xs = map mostSignificantBit (bitFieldSum xs) where
            mostSignificantBit x = if (fromIntegral x) >= (halfLength xs) then 1 else 0

-- Compute the least significant bit of the specified bit-field
leastSignificantBitField :: [BitField] -> BitField
leastSignificantBitField xs = map leastSignificantBit (bitFieldSum xs) where
            leastSignificantBit x = if (fromIntegral x) < (halfLength xs) then 1 else 0

-- Computes the most significant bit-field recursively
recurseBitField :: ([BitField] -> BitField) -> [BitField] -> BitField
recurseBitField f xs = recurseBitField' 0 xs where
    recurseBitField' _ [x] = x
    recurseBitField' p xs  = recurseBitField' (p + 1) xs' where
        xs'     = filter (\x -> (x !! p) == msb) xs
        msb     = (xsMsb !! p)
        xsSum   = bitFieldSum xs
        xsMsb   = f xs

-- The actual solver for the day 3 puzzle (part 1)
solvePart1 :: [BitField] -> Int
solvePart1 xs = gamma * epsilon where
    epsilon = toDecimal $ map (\b -> if b == 1 then 0 else 1) ys
    gamma   = toDecimal ys
    ys      = mostSignificantBitField xs

-- The actual solver for the day 3 puzzle (part 2)
solvePart2 :: [BitField] -> Int
solvePart2 xs = oxygen * co2 where
    oxygen  = toDecimal $ (recurseBitField mostSignificantBitField) xs
    co2     = toDecimal $ (recurseBitField leastSignificantBitField) xs

-- The day 3 puzzle
day3Solver :: IO [Int]
day3Solver = do
    input <- readInputFile inputFilePath bitfieldLineParser
    return [(solvePart1 input), (solvePart2 input)]