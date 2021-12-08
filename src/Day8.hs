module Day8 (day8Solver) where

import Common
import Data.Maybe
import Data.Function
import Data.List

-- A single pattern composed of signal patterns and outputs
newtype Pattern = Pattern ([[Char]], [[Char]]) deriving Show

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day8_input.txt"

-- A simple line parser for the patterns
patternLineParser :: String -> (Maybe Pattern)
patternLineParser s = Just $ Pattern (signals, outputs) where
    delimiter   = (split '|' s)
    signals     = words $ head delimiter
    outputs     = words $ last delimiter

-- Makes a new pattern given the two pattern sequences
mkPattern :: String -> Pattern
mkPattern = fromJust . patternLineParser where
    fromJust (Just x) = x
    fromJust _        = error "Failed to parse pattern"

-- Reads the inputs from the designated input file
readInputs :: IO [Pattern]
readInputs = readInputFile inputFilePath patternLineParser

-- The test input
testInput :: [Pattern]
testInput = [
    mkPattern "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    mkPattern "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    mkPattern "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    mkPattern "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    mkPattern "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    mkPattern "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    mkPattern "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    mkPattern "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    mkPattern "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    mkPattern "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    ]

-- Returns the segment pattern for the given digit
segmentsForDigit :: Int -> [Char]
segmentsForDigit 0 = "abcefg"
segmentsForDigit 1 = "cf"
segmentsForDigit 2 = "acdeg"
segmentsForDigit 3 = "acdfg"
segmentsForDigit 4 = "bcdf"
segmentsForDigit 5 = "abdfg"
segmentsForDigit 6 = "abdefg"
segmentsForDigit 7 = "acf"
segmentsForDigit 8 = "abcdefg"
segmentsForDigit 9 = "abcdfg"
segmentsForDigit _ = error "Only digits between 0 & 9 are supported"

-- Returns the segment patterns for digits [0..9]
segmentsToDigit :: [([Char], Int)]
segmentsToDigit = map (\n -> (segmentsForDigit n, n)) [0..9]

-- Returns the number of segments for the given digit
numberOfSegmentsForDigit :: Int -> Int
numberOfSegmentsForDigit = length . segmentsForDigit

countSimpleDigit :: [[Char]] -> Int
countSimpleDigit []     = 0
countSimpleDigit (o:os) = (countSimpleDigit os) + (if (any (==(length o)) simpleDigitSet) then 1 else 0) where
    simpleDigitSet      = map numberOfSegmentsForDigit [1, 4, 7, 8]

-- The actual solver for the day 8 puzzle (part 1)
solvePart1 :: [Pattern] -> Int
solvePart1 xs = foldr (\(Pattern (ss, os)) a -> a + (countSimpleDigit os)) 0 xs where

-- Decodes a single digit pattern given the resolved pattern
decodeDigit :: [([Char], Int)] -> [Char] -> Int
decodeDigit ds xs = case find (\(s, n) -> s == (sort xs)) ds of
    Just (s, n) -> n
    _           -> error "Unable to decode pattern"

-- Decodes a full n-digit number given the resolved pattern
decodeNumber :: [([Char], Int)] -> [[Char]] -> Int
decodeNumber ds xss = decodeNumber' ds 3 xss where
    decodeNumber' ds n []     = 0
    decodeNumber' ds n (x:xs) = (decodeDigit ds x) * 10^n + (decodeNumber' ds (n-1) xs)

-- Triages a segment pattern based on the number of segments
triageSegmentPattern :: [[Char]] -> [(Int, [[Char]])]
triageSegmentPattern ss = map combinePairs (groupPairs (sort patternPairs)) where
    patternPairs = map (\s -> (length s, s)) ss
    groupPairs   = groupBy ((==) `on` fst)
    combinePairs = (\l -> (fst . head $ l, map snd l))

-- Computes the number of segments in common
segmentsInCommon :: [Char] -> [Char] -> Int
segmentsInCommon xs ys = length (filter (\y -> any (==y) xs) ys)

-- Solves a segment pattern
solveSegmentPattern :: [[Char]] -> [([Char], Int)]
solveSegmentPattern ss = solveSegmentPattern' (triageSegmentPattern ss) where
    solveSegmentPattern' ts = uniquePatterns ++ solvedPatterns where
        uniquePatterns = filter (\(p, n) -> (length p) > 0) (concat (map uniquePatterns' ts)) where
            uniquePatterns' (2, [x]) = [(sort x,  1)]
            uniquePatterns' (4, [x]) = [(sort x,  4)]
            uniquePatterns' (3, [x]) = [(sort x,  7)]
            uniquePatterns' (7, [x]) = [(sort x,  8)]
            uniquePatterns' _        = []
        solvedPatterns = filter (\(p, n) -> (length p) > 0) (concat (map solvedPatterns' ts)) where
            fourthPattern   = (\(x, _) -> x) $ fromJust (find (\(_, n) -> n == 4) uniquePatterns)
            seventhPattern  = (\(x, _) -> x) $ fromJust (find (\(_, n) -> n == 7) uniquePatterns)

            solvedPatterns' (5, xs)  = map (disambiguate fourthPattern seventhPattern) xs where -- 2, 3, 5
                disambiguate fp sp x = case (segmentsInCommon x fp) + (segmentsInCommon x sp) of
                    4 -> (sort x, 2)
                    6 -> (sort x, 3)
                    5 -> (sort x, 5)
                    _ -> error "Failed to solve pattern"
            solvedPatterns' (6, xs)  = map (disambiguate fourthPattern seventhPattern) xs where -- 0, 6, 9
                disambiguate fp sp x = case (segmentsInCommon x fp) + (segmentsInCommon x sp) of
                    6 -> (sort x, 0)
                    5 -> (sort x, 6)
                    7 -> (sort x, 9)
                    _ -> error "Failed to solve pattern"[]
            solvedPatterns' _        = []

            
-- The actual solver for the day 8 puzzle (part 2)
solvePart2 :: [Pattern] -> Int
solvePart2 xs = sum (map decodePattern xs) where
    decodePattern (Pattern (ss, os)) = decodeNumber (solveSegmentPattern ss) os

-- The day 8 puzzle
day8Solver :: IO [Int]
day8Solver = do
    input <- readInputs
    --let input = testInput
    return $ [solvePart1 input, solvePart2 input]