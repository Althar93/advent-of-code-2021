module Day13 (day13Solver) where

import Common
import Data.Char
import Data.Ord
import Data.List
import Data.Maybe

-- A single dot
type Dot = (Int, Int)

-- A single fold
type Fold = (Int, Int)

-- An origami
type Origami = ([Dot], [Fold])

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day13_input.txt"

-- A simple origami parser
origamiLineParser :: String -> (Maybe Origami)
origamiLineParser [] = Nothing
origamiLineParser s  = case head s of 
    'f'         -> Just $ ([], [(x,y)]) where
        x    = if (s' !! 0) == "x" then read (s' !! 1) else 0
        y    = if (s' !! 0) == "y" then read (s' !! 1) else 0
        s'   = (split '=' ((words s) !! 2))
    otherwise   -> Just $ ([(x,y)], []) where
        x   = read (s' !! 0)
        y   = read (s' !! 1)
        s'  = split ',' s

-- Reads the inputs from the designated input file
readInputs :: IO Origami
readInputs = do
    inputs <- readInputFile inputFilePath origamiLineParser
    return $ foldr (\(p0, f0) (p, f) -> (p0++p, f0++f)) ([], []) inputs

-- A test input
testInput :: Origami
testInput = (
    [
        (6, 10),
        (0, 14),
        (9, 10),
        (0, 3),
        (10, 4),
        (4, 11),
        (6, 0),
        (6, 12),
        (4, 1),
        (0, 13),
        (10, 12),
        (3, 4),
        (3, 0),
        (8, 4),
        (1, 10),
        (2, 14),
        (8, 10),
        (9, 0)
    ],
    [
        (0, 7),
        (5, 0)
    ]
    )

-- Removes the duplicates
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

-- Draws the given origami
drawOrigami :: Origami -> IO ()
drawOrigami (ps, fs) = mapM_ (drawRow ps) [0..maxY] where
    maxY         = foldr (max . snd) 0 ps
    drawRow ps y = putStrLn $ map (drawCell ps y) [0..maxX] where
        maxX       = foldr (max . fst) 0 ps
        drawCell ps y x = case (find (==(x, y)) ps) of 
            Just _  -> '#'
            Nothing -> '.'

-- Folds the points of an origami
foldPoints :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
foldPoints (xf, 0) ps = removeDuplicates (map (foldPointsX xf) ps) where
    foldPointsX xf (x,y) = if x < xf then (x, y) else (2 * xf - x, y)
foldPoints (0, yf) ps = removeDuplicates (map (foldPointsY yf) ps) where
    foldPointsY yf (x,y) = if y < yf then (x, y) else (x, 2 * yf - y)
foldPoints _ _        = error "Non orthogonal folds not supported"

-- Folds the given origami
foldOrigami :: Origami -> Origami
foldOrigami (ps, [])        = (ps, [])
foldOrigami (ps, (f:fs))    = foldOrigami (foldPoints f ps, fs)

-- The actual solver for the day 13 puzzle (part 1)
solvePart1 :: Origami -> Int
solvePart1 (ps, fs) = length ps' where
    ps' = foldPoints (head fs) ps

-- The actual solver for the day 13 puzzle (part 2)
solvePart2 :: Origami -> Int
solvePart2 (ps, fs) = 0

-- The day 13 puzzle
day13Solver :: IO [Int]
day13Solver = do
    input <- readInputs
    --let input  = testInput
    drawOrigami (foldOrigami input)
    return [solvePart1 input]