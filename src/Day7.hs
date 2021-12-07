module Day7 (day7Solver) where

import Common
import Data.List

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day7_input.txt"

-- A simple line parser for the crab positions
crabPositionsParser :: String -> (Maybe [Int])
crabPositionsParser s = Just $ map read (split ',' s)

-- Reads the inputs from the designated input file
readInputs :: IO [Int]
readInputs = do
    inputs <- readInputFile inputFilePath crabPositionsParser
    return $ concat inputs

-- The test input
testInput :: [Int]
testInput = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

-- Computes the average
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / genericLength xs

-- Computes the variance
variance :: (Real a, Fractional b) => [a] -> b
variance xs = (sum (map (\x -> (realToFrac (x) - avg)^2) xs)) / genericLength xs where
    avg = average xs

-- Computes the standard deviation
standardDeviation :: (Real a, Floating b) => [a] -> b
standardDeviation = sqrt . variance

-- Compute the total fuel to move to a given position (for part 1)
computeFuelFor :: [Int] -> Int -> Int
computeFuelFor xs p = sum $ map (\x -> abs(x - p)) xs

-- Compute the total fuel to move to a given position (for part 2)
computeFuelFor' :: [Int] -> Int -> Int
computeFuelFor' xs p = sum $ map (\x -> inc (abs(x - p))) xs where
    inc 0 = 0
    inc n = n + inc (n - 1)

-- The actual solver for the day 7 puzzle (part 1)
solvePart1 :: [Int] -> Int
solvePart1 xs = (\(f, p) -> f) $ (head . sort) (map (\x -> (computeFuelFor xs x, x)) [minimum xs..maximum xs])

-- The actual solver for the day 7 puzzle (part 2)
solvePart2 :: [Int] -> Int
solvePart2 xs = (\(f, p) -> f) $ (head . sort) (map (\x -> (computeFuelFor' xs x, x)) [minimum xs..maximum xs])

-- The day 7 puzzle
day7Solver :: IO [Int]
day7Solver = do
    input <- readInputs
    --let input = testInput
    return $ [solvePart1 input, solvePart2 input]