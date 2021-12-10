module Day9 (day9Solver) where

import Common
import Data.Char
import Data.Ord
import Data.List
import Data.Maybe

-- A simple digit line parser
digitLineParser :: String -> (Maybe [Int])
digitLineParser s = Just $ map digitToInt s 

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day9_input.txt"

-- Reads the inputs from the designated input file
readInputs :: IO [[Int]]
readInputs = readInputFile inputFilePath digitLineParser

-- Test input
testInput :: [[Int]]
testInput = [
    [2,1,9,9,9,4,3,2,1,0],
    [3,9,8,7,8,9,4,9,2,1],
    [9,8,5,6,7,8,9,8,9,2],
    [8,7,6,7,8,9,6,7,8,9],
    [9,8,9,9,9,6,5,6,7,8]
    ]

-- Samples the map at the specified value
sampleMap :: [[a]] -> (Int, Int) -> Maybe a
sampleMap xs (nx, ny) | (ny < 0) || (ny > (length xs) - 1)          = Nothing
                      | (nx < 0) || (nx > (length (xs !! ny) - 1))  = Nothing
                      | otherwise                                   = Just $ ((xs !! ny) !! nx)

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort

-- Filters and returns the just elements of an array
filterFromJust :: [Maybe a] -> [a]
filterFromJust = (map fromJust) . (filter isJust)

-- Returns the neighbours for a given cell
neighbours :: [[a]] -> (a, Int, Int) -> [(a, Int, Int)]
neighbours hs (a0, x0, y0) = map (\(ma, x, y) -> (fromJust ma, x, y)) (filter (\(ma, _, _) -> isJust ma) neighboursM) where
    neighboursM = map (\(x, y) -> (sampleMap hs (x, y), x, y)) [(x0 - 1, y0), (x0 + 1, y0), (x0, y0 - 1), (x0, y0 + 1)]

-- Convolves a 2D map given the convolution function
convolveMap :: ([[a]] -> (a, Int, Int) -> b) -> [[a]] -> [[b]]
convolveMap _ [] = []
convolveMap f hs = (convolveMapY f hs 0) where
    convolveMapY _ [] _      = []
    convolveMapY f (y:ys) ny = (convolveMapX f y 0) : (convolveMapY f ys (ny + 1)) where
        convolveMapX _ [] _      = []
        convolveMapX f (x:xs) nx = (f hs (x, nx, ny)) : (convolveMapX f xs (nx + 1)) where

-- Returns the minima
minima :: Ord a => [[a]] -> (a, Int, Int) -> Maybe a
minima xs x0@(a0, _, _) = if (all (\(a, _, _) -> a > a0) (neighbours xs x0)) then (Just a0) else Nothing

-- Similar to minima, but returns the coordinates of the basin bottom
basinBottom :: Ord a => [[a]] -> (a, Int, Int) -> Maybe (a, Int, Int)
basinBottom xs x0@(a0, _, _) = if (all (\(a, _, _) -> a > a0) (neighbours xs x0)) then (Just x0) else Nothing

-- Computes the size of a given basin
computeBasinSize :: [a] -> Int
computeBasinSize = length

-- Compute a basin by walking up from a local minima
computeBasin :: (Num a, Ord a) => [[a]] -> (a, Int, Int) -> [(a, Int, Int)]
computeBasin hss h0 = removeDuplicates (computeBasin' hss h0) where
    computeBasin' _ (9, _, _)         = []
    computeBasin' hss h0@(a0, x0, y0) = h0 : (concat (map (computeBasin' hss) h1s)) where
        h1s = filter (\(a, x, y) -> a > a0) (neighbours hss h0)

-- The actual solver for the day 9 puzzle (part 1)
solvePart1 :: [[Int]] -> Int
solvePart1 xss = sum riskPoints where
    riskPoints  = map ((+1).fromJust) (filter isJust riskPointsM)
    riskPointsM = concat $ convolveMap minima xss

-- The actual solver for the day 9 puzzle (part 2)
solvePart2 :: [[Int]] -> Int
solvePart2 xss = product (take 3 (reverse (sort (map computeBasinSize basins)))) where
    basins       = map (computeBasin xss) basinBottoms
    basinBottoms = filterFromJust (concat (convolveMap basinBottom xss))

-- The day 9 puzzle
day9Solver :: IO [Int]
day9Solver = do
    input <- readInputs
    --let input = testInput
    return [solvePart1 input, solvePart2 input]