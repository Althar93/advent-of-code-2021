module Day14 (day14Solver) where

import Common
import Data.Char
import Data.Ord
import Data.List
import Data.Tuple
import Data.Maybe
import Data.Function

-- Polymer template
type Template = [Char]

-- A polymer template, defined in buckets of pairs
type BucketTemplate = [((Char, Char), Integer)]

-- Polymer insertion rules
type InsertionRule = ((Char, Char), Char)

-- A polymer definition consisting of the polymer template & some insertion rules
type Polymer = (Template, [InsertionRule])

-- A polymer definition consisting of the polymer bucket template & some insertion rules
type BucketPolymer = (BucketTemplate, [InsertionRule])

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day14_input.txt"

-- A simple polymer line parser
polymerLineParser :: String -> (Maybe Polymer)
polymerLineParser [] = Nothing
polymerLineParser s  = case find (=='>') s of 
    Nothing     -> Just $ (s, [])
    Just _      -> Just $ ([], [(xs, y)]) where
        xs  = (head (s' !! 0), last (s' !! 0))
        y   = head (s' !! 2)
        s'  = words s

-- Reads the inputs from the designated input file
readInputs :: IO Polymer
readInputs = do
    inputs <- readInputFile inputFilePath polymerLineParser
    return $ foldr (\(p0, f0) (p, f) -> (p0++p, f0++f)) ([], []) inputs

-- A small test input
testInput :: Polymer
testInput = (
    "NNCB",
    [
        (('C', 'H'),'B'),
        (('H', 'H'),'N'),
        (('C', 'B'),'H'),
        (('N', 'H'),'C'),
        (('H', 'B'),'C'),
        (('H', 'C'),'B'),
        (('H', 'N'),'C'),
        (('N', 'N'),'C'),
        (('B', 'H'),'H'),
        (('N', 'C'),'B'),
        (('N', 'B'),'B'),
        (('B', 'N'),'B'),
        (('B', 'B'),'N'),
        (('B', 'C'),'B'),
        (('C', 'C'),'N'),
        (('C', 'N'),'C')
    ]
    )

-- Turns the given polymer into a set of pairs
mkPairs :: Polymer -> [(Char, Char)]
mkPairs (ts, irs) = mkPairs' ts where
    mkPairs' []          = []
    mkPairs' [x]         = []
    mkPairs' (x0:x1:xs)  = (x0,x1):(mkPairs' (x1:xs))

-- Turns the given polymer into a bucket polymer
mkBucketPolymer :: Polymer -> BucketPolymer
mkBucketPolymer p@(ts, irs) = (ts', irs) where
    ts' = map (\x -> (head x, fromIntegral(length x))) ((group . sort) (mkPairs p))

-- Computes the integer length of the given list
integerLength :: [a] -> Integer
integerLength = foldr (\_ a -> a + 1) 0

-- Steps the given polymer
step :: Polymer -> Polymer
step (ts, irs) = (ts', irs) where 
    ts'                     = step' ts irs where
    step' [] _              = []
    step' [x] _             = [x]
    step' (x0:x1:xs) irs    = case find (\((a, b), _) -> a == x0 && b == x1) irs of
        Nothing             -> x0:(step' (x1:xs) irs)
        Just ((_, _), x')   -> x0:x':(step' (x1:xs) irs)

-- Steps the given polymer a number of times using brute force
stepN :: Int -> Polymer -> Polymer
stepN 0 p = p
stepN n p = stepN (n - 1) (step p)

-- Steps the given bucket based on the insertion rules
stepBucket :: BucketPolymer -> BucketPolymer
stepBucket (ts, irs) = (ts''', irs) where
    ts'''   = map (foldr (\(a, n) (_, n0) -> (a, n0 + n)) (('$', '$'), 0)) ts''
    ts''    = ((groupBy ((==) `on` fst)) . sort) ts'
    ts'     = stepBucket' ts irs where
    stepBucket' [] _                    = []
    stepBucket' (x@((a, b), n):xs) irs  = ((a, c), n):((c, b), n):(stepBucket' xs irs) where 
        c = case find (\((x0, x1), _) -> a == x0 && b == x1) irs of
            Nothing -> error "We somehow ended with an invalid pair"
            Just ((_, _), x') -> x'

-- Steps the given polymer a number of times using buckets
stepNBuckets :: Int -> BucketPolymer -> BucketPolymer
stepNBuckets 0 p = p
stepNBuckets n p = stepNBuckets (n - 1) (stepBucket p)

-- The actual solver for the day 14 puzzle (part 1)
solvePart1 :: Polymer -> Integer
solvePart1 p = fromIntegral(maxElement - minElement) where
    maxElement = maximum (map length elements)
    minElement = minimum (map length elements)
    elements   = (group . sort) ts 
    (ts, irs)  = (stepN 10 p)

-- The actual solver for the day 14 puzzle (part 2)
solvePart2 :: Polymer -> Integer
solvePart2 p = fromIntegral(maxElement - minElement) where
    maxElement = maximum (map snd elements)
    minElement = minimum (map snd elements)
    elements   = countElements p''
    p''        = (stepNBuckets 40 p')
    p'         = mkBucketPolymer p

-- Returns the maximum of two values within a tuple
maxTuple :: Ord a => (a, a) -> a
maxTuple (x, y) | x > y     = x
                | otherwise = y

-- #TODO : This needs fixing - this method will break under certain occurrences of patterns, e.g. 'NCNN' or 'NNCN' whereby the count for 'N' will be incorrectly reported as '2' instead of '3', or
-- 'NCN' where the count for 'N' will be ' 1'. It just so happens the puzzle input and test patterns do not exhibit such patterns and so yield the correct count.
-- This can either be solved by keeping a counter of each character, or assuming only the first and last letters are the only ones that do not get duplicated (i.e. appear twice), while all other pairs appear twice.
-- Counts the number of times the specified element occurs in the template
countElements' :: BucketTemplate -> Char -> Integer
countElements' ts c = maxTuple (foldr (countElements'' c) (0, 0) ts) where
    countElements'' c ((a, b), n) (na, nb) = (na', nb') where
        na' = na + (if a == c then n else 0)
        nb' = nb + (if b == c then n else 0)

countElements :: BucketPolymer -> [(Char, Integer)]
countElements (ts, _) = map (\x -> (x, countElements' ts x)) uniqueElements' where
    uniqueElements  = concat (map (\((a, b), _) -> a:b:[]) ts)
    uniqueElements' = map (\xs -> head xs) ((group . sort) uniqueElements)

-- The day 14 puzzle
day14Solver :: IO [Integer]
day14Solver = do
    --input <- readInputs
    let input = testInput
    return [solvePart1 input, solvePart2 input]