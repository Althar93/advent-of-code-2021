module Day2 (day2Solver) where

import Common

-- A command constituting of an x & y displacement amount
data Command = Down Int | Up Int | Forward Int

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day2_input.txt"

-- A basic line parser
commandLineParser :: String -> (Maybe Command)
commandLineParser xs = commandLineParser' (words xs) where
    commandLineParser' [direction, amount]  = mapCommand direction (read amount) where
        mapCommand "forward" n  = Just $ Forward n
        mapCommand "up" n       = Just $ Up n
        mapCommand "down" n     = Just $ Down n
        mapCommand _ _          = Nothing
    commandLineParser' _        = Nothing

-- The test input
testInput :: [Command]
testInput = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]

-- The actual solver for the day 2 puzzle (part 1)
solvePart1 :: [Command] -> Int
solvePart1 xs = condense (step xs (0,0)) where
    condense (x, y)             = x * y
    step (c:xs) (x0, y0)        = case c of Forward n -> step xs (x0 + n, y0)
                                            Down n    -> step xs (x0, y0 + n)
                                            Up n      -> step xs (x0, y0 - n)
    step _ (x, y)               = (x, y)

-- The actual solver for the day 2 puzzle (part 2)
solvePart2 :: [Command] -> Int
solvePart2 xs = condense (step xs (0,0,0)) where
    condense (x, y, z)          = x * y
    step (c:xs) (x0, y0, z0)    = case c of Forward n -> step xs (x0 + n, y0 + n * z0, z0)
                                            Down n    -> step xs (x0, y0, z0 + n)
                                            Up n      -> step xs (x0, y0, z0 - n)
    step _ (x, y, z)            = (x, y, z)

-- The day 2 puzzle
day2Solver :: IO [Int]
day2Solver = do
    input <- readInputFile inputFilePath commandLineParser
    --let input = testInput
    return [(solvePart1 input), (solvePart2 input)]