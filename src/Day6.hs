module Day6 (day6Solver) where

import Common

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day6_input.txt"

inputSequence :: [Int]
inputSequence = [
    5,4,3,5,1,1,2,1,2,1,3,2,3,
    4,5,1,2,4,3,2,5,1,4,2,1,1,
    2,5,4,4,4,1,5,4,5,2,1,2,5,
    5,4,1,3,1,4,2,4,2,5,1,3,5,
    3,2,3,1,1,4,5,2,4,3,1,5,5,
    1,3,1,3,2,2,4,1,3,4,3,3,4,
    1,3,4,3,4,5,2,1,1,1,4,5,5,
    1,1,3,2,4,1,2,2,2,4,1,2,5,
    5,1,4,5,2,4,2,1,5,4,1,3,4,
    1,2,3,1,5,1,3,4,5,4,1,4,3,
    3,3,5,5,1,1,5,1,5,5,1,5,2,
    1,5,1,2,3,5,5,1,3,3,1,5,3,
    4,3,4,3,2,5,2,1,2,5,1,1,1,
    1,5,1,1,4,3,3,5,1,1,1,4,4,
    1,3,3,5,5,4,3,2,1,2,2,3,4,
    1,5,4,3,1,1,5,1,4,2,3,2,2,
    3,4,1,3,4,1,4,3,4,3,1,3,3,
    1,1,4,1,1,1,4,5,3,1,1,2,5,
    2,5,1,5,3,3,1,3,5,5,1,5,4,
    3,1,5,1,1,5,5,1,1,2,5,5,5,
    1,1,3,2,2,3,4,5,5,2,5,4,2,
    1,5,1,4,4,5,4,4,1,2,1,1,2,
    3,5,5,1,3,1,4,2,3,3,1,4,1,1]

splitSequence :: [Int] -> Int -> [[Int]]
splitSequence [] _ = []
splitSequence xs n = (take n xs) : (splitSequence (drop n xs) n)

testSequence :: [Int]
testSequence = [3,4,3,1,2]

-- Executes a single tick
tickFish :: [Int] -> Integer -> ([Int], Integer)
tickFish [] n                  = ([], n)
tickFish (x:xs) n | (x <= 0)   = (\(xs, n) -> ([6, 8] ++ xs, n + 1)) (tickFish xs n)
                  | otherwise  = (\(xs, n) -> ((x - 1) : xs, n))     (tickFish xs n)

-- Simulates a fish growth population for a given number of days
simulateFish :: Int -> [Int] -> ([Int], Integer)
simulateFish d xs = simulateFish' d xs (toInteger (length xs)) where
    simulateFish' d xs n | d <= 0     = (xs, n)
                         | otherwise  = simulateFish' (d - 1) xs' n' where
                            (xs', n') = tickFish xs n

-- The actual solver for the day 6 puzzle (part 1)
solvePart1BruteForce :: [Int] -> Integer
solvePart1BruteForce xs = snd (simulateFish 80 xs) 

-- Adds fish to an initial bucket
addFishToBuckets :: Int -> [Integer] -> [Integer]
addFishToBuckets a xs = addFishToBuckets' a xs 0 where
    addFishToBuckets' _ [] _     = []
    addFishToBuckets' a (x:xs) n = (if (n == a) then (x + 1) else x) : (addFishToBuckets' a xs (n + 1))

-- Ticks the pool of fish (i.e. age the fish & spawn new ones)
tickFishPool :: [Integer] -> [Integer]
tickFishPool xs = (x0 : x1 : x2 : x3 : x4 : x5 : x6 : x7 : x8 : []) where
    x0 = xs !! 1
    x1 = xs !! 2
    x2 = xs !! 3
    x3 = xs !! 4
    x4 = xs !! 5
    x5 = xs !! 6
    x6 = (xs !! 7) + (xs !! 0)
    x7 = xs !! 8
    x8 = xs !! 0

-- Simulates a fish growth population for a given number of days
simulateFishPool :: Int -> [Integer] -> Integer
simulateFishPool d ps = simulateFishPool' d ps where
    simulateFishPool' d ps  | (d <= 0)  = sum ps
                            | otherwise = simulateFishPool' (d - 1) (tickFishPool ps)

-- Much more efficient solution - just place fish in buckets of age
solvePart2Bucket :: [Int] -> Integer
solvePart2Bucket xs = simulateFishPool 256 buckets where
    buckets = foldr addFishToBuckets (take 9 (repeat 0)) xs

-- The day 6 puzzle
day6Solver :: IO [Integer]
day6Solver = do
    let input = inputSequence
    return [solvePart1BruteForce input, solvePart2Bucket input]

