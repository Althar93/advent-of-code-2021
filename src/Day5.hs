module Day5 (day5Solver) where

import Common
import Data.List

-- A basic point in 2d space
type Point = (Int, Int)

-- A vent line
type VentLine = (Point, Point)

testInput :: [VentLine]
testInput = [
    ((0, 9), (5, 9)),
    ((8, 0), (0, 8)),
    ((9, 4), (3, 4)),
    ((2, 2), (2, 1)),
    ((7, 0), (7, 4)),
    ((6, 4), (2, 0)),
    ((0, 9), (2, 9)),
    ((3, 4), (1, 4)),
    ((0, 0), (8, 8)),
    ((5, 5), (8, 2))]

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day5_input.txt"

-- A basic line parser
ventLineParser :: String -> (Maybe VentLine)
ventLineParser [] = Nothing
ventLineParser xs = Just (p0, p1) where
        p0          = readPoint (tokens !! 0)
        p1          = readPoint (tokens !! 2)
        tokens      = words xs
        readPoint y = (u, v) where
            u = read $ head (split ',' y)
            v = read $ last (split ',' y)


orthoPoints :: VentLine -> [(Int, Int)]
orthoPoints ((x0, y0), (x1, y1)) | (x0 == x1) = [(x0, y) | y <- [(min y0 y1)..(max y0 y1)] ] 
                                 | (y0 == y1) = [(x, y0) | x <- [(min x0 x1)..(max x0 x1)] ] 
                                 | otherwise  = []             

points :: VentLine -> [(Int, Int)]
points v@((x0, y0), (x1, y1)) = case orthoPoints v of 
    [] -> [(x, minY + round (fromIntegral (x - minX) * gradient)) | x <- [minX..maxX]] where
        (minX, minY) = if x0 < x1 then (x0, y0) else (x1, y1)
        (maxX, maxY) = if x0 > x1 then (x0, y0) else (x1, y1)
        gradient     = fromIntegral (maxY - minY) / fromIntegral (maxX - minX)
    xs -> xs

drawLine :: VentLine -> [[Int]] -> (VentLine -> [(Int, Int)]) -> [[Int]]
drawLine v ys p = drawLine' v p 0 ys where
    drawLine' _ _ _ []       = []
    drawLine' v p ny (x:ys)  = (drawRow v p 0 x) : (drawLine' v p (ny + 1) ys) where
        drawRow _ _ _ []      = []
        drawRow v p nx (x:xs) = (drawCell v p nx ny x) : (drawRow v p (nx + 1) xs) where
            drawCell v p nx ny x = if (isOnLine v nx ny) then (x + 1) else x
            isOnLine v@((x0, y0), (x1, y1)) nx ny = any (\(x, y) -> x == nx && y == ny) (p v) where

-- Builds a grid of overlapping lines
buildOverlapGrid :: (VentLine -> [(Int, Int)]) -> [VentLine] -> [[Int]]
buildOverlapGrid p vs = buildOverlapGrid' p vs initialGrid where
    buildOverlapGrid' _ [] g        = g
    buildOverlapGrid' p (v:vs) g    = buildOverlapGrid' p vs (drawLine v g p)
    initialGrid                 = take (maxY + 1) (repeat (take (maxX + 1) (repeat 0)))
    maxX                        = foldr (\((x0, _), (x1, _)) a -> max x0 (max x1 a)) 0 vs
    maxY                        = foldr (\((_, y0), (_, y1)) a -> max y0 (max y1 a)) 0 vs

-- Build the list of points
buildPointList :: (VentLine -> [(Int, Int)]) -> [VentLine] -> [(Int, Int)]
buildPointList _ []     = []
buildPointList p (v:vs) = (p v) ++ buildPointList p vs

-- Count the number of occurences of a given entry in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

-- Removes the duplicates
removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map head . group . sort

drawGrid :: [[Int]] -> IO ()
drawGrid []     = return ()
drawGrid (x:xs) = do
    putStrLn $ show x
    drawGrid xs

-- Returns whether a vent line is orthogonal
isOrthogonal :: VentLine -> Bool
isOrthogonal ((x0, y0), (x1, y1)) = (x0 == x1) || (y0 == y1)

-- The actual solver for the day 5 puzzle (part 1)
solvePart1 :: [VentLine] -> Int
solvePart1 xs = length overalappingLines where
    overalappingLines = filter (\n -> n >= 2) (map (\x -> count x pointList) uniquePointList)
    pointList       = (buildPointList orthoPoints xs)
    uniquePointList = removeDuplicates pointList

-- The actual solver for the day 5 puzzle (part 2)
solvePart2 :: [VentLine] -> Int
solvePart2 xs = length overalappingLines where
    overalappingLines = filter (\n -> n >= 2) (map (\x -> count x pointList) uniquePointList)
    pointList       = (buildPointList points xs)
    uniquePointList = removeDuplicates pointList

-- The day 5 puzzle
day5Solver :: IO [Int]
day5Solver = do
    input <- readInputFile inputFilePath ventLineParser
    return $ [solvePart1 input, solvePart2 input]
