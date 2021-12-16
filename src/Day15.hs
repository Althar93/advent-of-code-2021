module Day15 (day15Solver) where

import Common
import Data.Char
import Data.Ord
import Data.List
import Data.Maybe

-- A simple point
type Point = (Int, Int)

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day15_input.txt"

-- A simple digit parser
digitLineParser :: String -> (Maybe [Int])
digitLineParser s = Just $ map digitToInt s 

-- Reads the inputs from the designated input file
readInputs :: IO [[Int]]
readInputs = readInputFile inputFilePath digitLineParser

-- Some test input
testInput :: [[Int]]
testInput = [
    [1 ,1, 6, 3, 7, 5, 1, 7, 4, 2],
    [1 ,3, 8, 1, 3, 7, 3, 6, 7, 2],
    [2 ,1, 3, 6, 5, 1, 1, 3, 2, 8],
    [3 ,6, 9, 4, 9, 3, 1, 5, 6, 9],
    [7 ,4, 6, 3, 4, 1, 7, 1, 1, 1],
    [1 ,3, 1, 9, 1, 2, 8, 1, 3, 7],
    [1 ,3, 5, 9, 9, 1, 2, 4, 2, 1],
    [3 ,1, 2, 5, 4, 2, 1, 6, 3, 9],
    [1 ,2, 9, 3, 1, 3, 8, 5, 2, 1],
    [2 ,3, 1, 1, 9, 4, 4, 5, 8, 1]
    ]

-- Some test input
testInput2 :: [[Int]]
testInput2 = [
    [1 ,1, 6],
    [1 ,3, 8],
    [2 ,1, 3]
    ]

-- Returns whether a given point is inside the map
isInside :: [[a]] -> Point -> Bool
isInside xss (x, y) | (y < 0) || (y > (length xss) - 1)         = False
                    | (x < 0) || (x > (length (xss !! y) - 1))  = False
                    | otherwise                                 = True

-- Returns the neighbours for a given cell - we cheat a bit for this puzzle by assuming that we never travel backwards or upwards.
neighbours :: [[a]] -> Point -> [Point]
neighbours xss (x, y) = filter (isInside xss) [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

-- A node as a linked chain 
data Node = Node {
    pos     :: !Point,
    risk    :: !Int,
    from    :: !(Maybe Node)
}

-- Make our node an instance of eq so we can compare it against others in a list
instance Eq Node where
    (==) a b = (pos a) == (pos b)

-- Convenience function for making nodes
mkNode :: Point -> Int -> Maybe Node -> Node
mkNode p r mp = Node { pos=p, risk=r, from=mp }

-- Unfolds the node giving the full list of nodes along the chain
unfoldNode :: Node -> [Node]
unfoldNode n = case (from n) of
    Nothing -> [n]
    Just p  -> (unfoldNode p) ++ [n]

-- Returns the risk cost of a given point
riskCost :: [[Int]] -> Point -> Int
riskCost xss (x, y) = (xss !! y) !! x

-- Draws the given grid
drawGrid :: [[Int]] -> IO ()
drawGrid xss = mapM_ (drawRow xss) [0..maxY] where
    maxY         = (length xss) - 1
    drawRow xss y = putStrLn $ map (drawCell xss y) [0..maxX] where
        maxX       = (length (xss !! y)) - 1
        drawCell ps y x = intToDigit $ (xss !! y) !! x

-- A* algorithm - returns the shortest path but isn't particularly fast for part 2...
astar :: [[Int]] -> Point -> [Node] -> [Node] -> Maybe Node
astar _ _ [] _    = Nothing
astar xss e os cs = if ((pos p) == e) then (Just p) else (astar xss e os' cs') where
    cs'     = p:cs
    os'     = foldl queue (delete p os) (neighbours xss (pos p))
    queue a xy = case find (\n -> (pos n) == xy) (os ++ cs) of
        Nothing -> (mkNode xy ((riskCost xss xy) + (risk p)) (Just p)) : a
        Just _  -> a
    p       = head (sortBy (\a b -> (risk a) `compare` (risk b)) os)

-- Computes the path & associated risk using A*
computePathAstar :: [[Int]] -> Point -> Point -> ([Point], Int)
computePathAstar xss s e = (ps, r) where
    p  = fromJust $ astar xss e [(mkNode s 0 Nothing)] []
    ps = map pos (unfoldNode p)
    r  = (risk p)

-- Expands the specified area horizontally and vertically
expandArea :: [[Int]] -> Int -> [[Int]]
expandArea xss 0 = xss
expandArea xss n = map (populateRow xss) [0..(h * n) - 1] where
    h  = (length xss)
    populateRow xss y = map (populateCell xss y) [0..(w * n) - 1] where
        w = (length (xss !! 0))
        populateCell xss y x = inc ((x `div` w) + (y `div` h)) (xss !! (y `mod` w) !! (x `mod` h))
        inc 0 n = n
        inc n 9 = inc (n - 1) 1
        inc n v = inc (n - 1) (v + 1)

-- The actual solver for the day 15 puzzle (part 1)
solvePart1 :: [[Int]] -> Int
solvePart1 xss = snd (computePathAstar xss start end) where
   start = (0, 0)
   end   = (length (head xss) - 1, (length xss) - 1)

-- The actual solver for the day 15 puzzle (part 2)
solvePart2 :: [[Int]] -> Int
solvePart2 xss = snd (computePathAstar xss' start end) where
   xss'  = expandArea xss 5
   start = (0, 0)
   end   = (length (head xss') - 1, (length xss') - 1)

-- The day 15 puzzle
day15Solver :: IO [Int]
day15Solver = do
    --input <- readInputs
    let input = testInput
    let input' = expandArea input 0
    return [solvePart1 input, solvePart2 input]