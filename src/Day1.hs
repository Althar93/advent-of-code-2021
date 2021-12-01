module Day1 (day1Solver) where

import Common

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day1_input.txt"

-- The test input
testInput :: [Int]
testInput = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

-- The actual solver for the day 1 puzzle (part 1)
solvePart1 :: [Int] -> Int
solvePart1 xs = solve' xs 0 where
    solve' (x:y:xs) n   = solve' (y:xs) (if (y > x) then n + 1 else n)
    solve' _ n          = n

-- The actual solver for the day 1 puzzle (part 2) : same as part 1 but with a sliding window of three sums
solvePart2 :: [Int] -> Int
solvePart2 xs = solvePart1 (computeSlidingWindowSum xs) where
    computeSlidingWindowSum (x:y:z:xs) = (x + y + z) : computeSlidingWindowSum(y:z:xs)
    computeSlidingWindowSum _          = []

-- The day 1 puzzle
day1Solver :: IO [Int]
day1Solver = do
    input <- readInputFile inputFilePath readLineParser
    return [(solvePart1 input), (solvePart2 input)]

