module Day11 (day11Solver) where

import Common
import Data.Char
import Data.Ord
import Data.List
import Data.Maybe

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day11_input.txt"

-- A simple digit parser
digitLineParser :: String -> (Maybe [Int])
digitLineParser s = Just $ map digitToInt s 

-- Reads the inputs from the designated input file
readInputs :: IO [[Int]]
readInputs = readInputFile inputFilePath digitLineParser

-- Test input
testInput :: [[Int]]
testInput = [
    [5,4,8,3,1,4,3,2,2,3],
    [2,7,4,5,8,5,4,7,1,1],
    [5,2,6,4,5,5,6,1,7,3],
    [6,1,4,1,3,3,6,1,4,6],
    [6,3,5,7,3,8,5,4,7,8],
    [4,1,6,7,5,2,4,6,4,5],
    [2,1,7,6,8,4,1,7,2,1],
    [6,8,8,2,8,8,1,1,3,4],
    [4,8,4,6,8,4,8,5,5,4],
    [5,2,8,3,7,5,1,5,2,6]
    ]

-- A smaller test input
smallTestInput :: [[Int]]
smallTestInput = [
    [1,1,1,1,1],
    [1,9,9,9,1],
    [1,9,1,9,1],
    [1,9,9,9,1],
    [1,1,1,1,1]
    ]

-- Convenient way to draw a grid
drawGrid :: Show a => [[a]] -> IO ()
drawGrid = mapM_ (putStrLn . show)

-- Samples the map at the specified value
sampleMap :: [[a]] -> (Int, Int) -> Maybe a
sampleMap xs (nx, ny) | (ny < 0) || (ny > (length xs) - 1)          = Nothing
                      | (nx < 0) || (nx > (length (xs !! ny) - 1))  = Nothing
                      | otherwise                                   = Just $ ((xs !! ny) !! nx)

-- The neighbour indices
neighbourIndices :: (Int, Int) -> [(Int, Int)]
neighbourIndices (x,y) = [
        (x - 1, y - 1), 
        (x,     y - 1), 
        (x + 1, y - 1), 
        (x - 1, y    ), 
        (x + 1, y    ),
        (x - 1, y + 1), 
        (x,     y + 1), 
        (x + 1, y + 1)
    ]

-- Returns the neighbours for a given cell
neighbours :: [[a]] -> (a, Int, Int) -> [(a, Int, Int)]
neighbours hs (a0, x0, y0) = map (\(ma, x, y) -> (fromJust ma, x, y)) (filter (\(ma, _, _) -> isJust ma) neighboursM) where
    neighboursM = map (\(x, y) -> (sampleMap hs (x, y), x, y)) (neighbourIndices (x0, y0))

-- Convolves a 2D map given the convolution function
convolveMap :: ([[a]] -> (a, Int, Int) -> b) -> [[a]] -> [[b]]
convolveMap _ [] = []
convolveMap f hs = (convolveMapY f hs 0) where
    convolveMapY _ [] _      = []
    convolveMapY f (y:ys) ny = (convolveMapX f y 0) : (convolveMapY f ys (ny + 1)) where
        convolveMapX _ [] _      = []
        convolveMapX f (x:xs) nx = (f hs (x, nx, ny)) : (convolveMapX f xs (nx + 1)) where

-- Increases the energy
increaseEnergy :: [[Int]] -> (Int, Int, Int) -> Int
increaseEnergy _ (e, _, _) = (e + 1)

-- Inits the octopus flash flag
initOctopusFlash :: [[Int]] -> (Int, Int, Int) -> (Int, Bool)
initOctopusFlash _ (e, _, _) = (e, False)

-- Flash
flashOctopus :: [[(Int, Bool)]] -> ((Int, Bool), Int, Int) -> (Int, Bool)
flashOctopus xss x@((e, b), _, _) = if shouldFlash then (e', True) else (e', b) where 
    e'              = (e + neighbourEnergy)
    shouldFlash     = (e > 9) && (b == False)
    neighbourEnergy = length (filter (\((ne, be), _, _) -> (ne > 9) && (be == False)) (neighbours xss x))

-- Reset any octopus that has flashed back to 0 & count the flash
resetOctopus :: [[(Int, Bool)]] -> ((Int, Bool), Int, Int) -> (Int, Int)
resetOctopus xss x@(a, _, _) = if hasFlashed then (0, 1) else (e, 0) where 
    e               = fst a
    hasFlashed      = snd a

-- Recursively flash the octopus until no more flashes are detected
flashRecursive :: [[Int]] -> [[(Int, Bool)]]
flashRecursive xss = flashRecursive' (convolveMap initOctopusFlash xss) 0 where
    flashRecursive' xss n = if (n' == n) then xss else flashRecursive' xss' n' where
        n'    = sum (concat (map (map (\(_, b) -> if b then 1 else 0)) xss'))
        xss'  = convolveMap flashOctopus xss

-- A single step
step :: [[Int]] -> ([[Int]], Int)
step xss = (xss'''', numberOfFlashes) where
    xss''''         = map (map fst) xss'''
    numberOfFlashes = sum (concat (map (map snd) xss'''))
    xss'''          = convolveMap resetOctopus xss''
    xss''           = flashRecursive xss'
    xss'            = convolveMap increaseEnergy xss

-- The actual solver for the day 11 puzzle (part 1)
solvePart1 :: [[Int]] -> Int
solvePart1 xss = snd (solvePart' (xss, 0) 100) where
    solvePart' (xs, nf) n | n == 0      = (xs, nf)
                          | otherwise   = solvePart' (xs', nf + nf') (n - 1) where
                            (xs', nf') = step xs

-- The actual solver for the day 11 puzzle (part 2)
solvePart2 :: [[Int]] -> Int
solvePart2 xs = 1 + (solvePart2' xs 0) where
    solvePart2' xs n = if (numberOfFlashes == totalFlashes) then n else solvePart2' xs' (n + 1) where
        numberOfFlashes = nf'
        totalFlashes    = sum (concat (map (map (\_ -> 1)) xs))
        (xs', nf')      = step xs

-- The day 11 puzzle
day11Solver :: IO [Int]
day11Solver = do
    input <- readInputs
    --let input = testInput
    return [solvePart1 input, solvePart2 input]