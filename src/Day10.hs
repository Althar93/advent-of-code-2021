module Day10 (day10Solver) where

import Common
import Data.Char
import Data.Ord
import Data.List
import Data.Maybe

-- A simple digit line parser
chunkLineParser :: String -> (Maybe [Char])
chunkLineParser s = Just $ s

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day10_input.txt"

-- Reads the inputs from the designated input file
readInputs :: IO [[Char]]
readInputs = readInputFile inputFilePath chunkLineParser

-- Test input
testInput :: [[Char]]
testInput = [
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
    ]

-- The pair of opening and closing symbols
symbolPairs :: [(Char, Char)]
symbolPairs = [
    ('(', ')'),
    ('[', ']'),
    ('{', '}'),
    ('<', '>')
    ]

-- Returns whether the symbol is an opening symbol
isOpeningSymbol :: Char -> Bool
isOpeningSymbol x = isOpeningSymbol' x symbolPairs where
    isOpeningSymbol' _ []             = False
    isOpeningSymbol' x ((o, c):xs)    = if (x == o) then True else (isOpeningSymbol' x xs)

-- Returns whether the symbol is a closing symbol
isClosingSymbol :: Char -> Bool
isClosingSymbol x = isClosingSymbol' x symbolPairs where
    isClosingSymbol' _ []             = False
    isClosingSymbol' x ((o, c):xs)    = if (x == c) then True else (isClosingSymbol' x xs)

-- Returns the closing symbol matching with the specified opening symbol
closingSymbol :: Char -> Char
closingSymbol x = closingSymbol' x symbolPairs where
    closingSymbol' _ []              = error "No matching closing symbol"
    closingSymbol' x ((o, c):xs)    = if (x == o) then c else (closingSymbol' x xs)

-- Checks the line for errors and returns the first one
checkLineForErrors :: [Char] -> [(Char, Int)]
checkLineForErrors xs = checkLineForErrors' xs [] 0 where
    checkLineForErrors' [] _ _           = []
    checkLineForErrors' (x:xs) [] _      = checkLineForErrors' xs [x] 1
    checkLineForErrors' (x:xs) (y:ys) n  | (isClosingSymbol x) && (isOpeningSymbol y) = if ((closingSymbol y) /= x) then [(x, n)] else (checkLineForErrors' xs ys (n + 1))
                                         | (isOpeningSymbol x) = checkLineForErrors' xs (x:y:ys) (n + 1)

-- Completes the given line
completeLine :: [Char] -> [Char]
completeLine xs = (map closingSymbol (completeLine' xs [])) where
    completeLine' [] ys         = ys
    completeLine' (x:xs) []     = completeLine' xs [x]
    completeLine' (x:xs) (y:ys) | (isClosingSymbol x) && (isOpeningSymbol y) = if ((closingSymbol y) /= x) then (error "Can't complete corrupt line") else (completeLine' xs ys)
                                | (isOpeningSymbol x) = completeLine' xs (x:y:ys)

-- The actual solver for the day 10 puzzle (part 1)
solvePart1 :: [[Char]] -> Int
solvePart1 xs = computeScore where
    computeScore        = sum (map scoreForSymbol errorSymbols)
    errorSymbols        = map (\(c, _) -> c) errors
    errors              = concat $ map checkLineForErrors xs
    scoreForSymbol ')'  = 3
    scoreForSymbol ']'  = 57
    scoreForSymbol '}'  = 1197
    scoreForSymbol '>'  = 25137
    scoreForSymbol _    = error "Unsupported symbol"

-- Returns the middle element of an array
takeMiddle :: [a] -> a
takeMiddle [] = error "Invalid for empty arrays"
takeMiddle xs = xs !! (length xs `div` 2)

-- The actual solver for the day 10 puzzle (part 2)
solvePart2 :: [[Char]] -> Int
solvePart2 xs = computeScore where
    incompleteLines         = filter (\x -> (length (checkLineForErrors x)) == 0) xs
    completedLines          = map completeLine incompleteLines
    computeScore            = takeMiddle (sort (map (computeLineScore . reverse) completedLines))
    computeLineScore []     = 0
    computeLineScore (x:xs) = (scoreForSymbol x) + 5 * (computeLineScore xs)
    scoreForSymbol ')'      = 1
    scoreForSymbol ']'      = 2
    scoreForSymbol '}'      = 3
    scoreForSymbol '>'      = 4
    scoreForSymbol _        = error "Unsupported symbol"

-- The day 10 puzzle
day10Solver :: IO [Int]
day10Solver = do
    input <- readInputs
    --let input = testInput
    return [solvePart1 input, solvePart2 input]