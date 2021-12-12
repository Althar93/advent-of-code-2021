module Day12 (day12Solver) where

import Common
import Data.Char
import Data.Ord
import Data.List
import Data.Maybe

-- A single path
type Path = (String, String)

-- An itinerary (i.e. a tree of paths)
data Itinerary a = Single a | Branch a [Itinerary a]

instance (Show a) => Show (Itinerary a) where
    show (Single a)     = "(" ++ show a ++ ")"
    show (Branch a as)  = "(" ++ show a ++ "," ++ show as ++ ")" 

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day12_input.txt"

-- A simple digit parser
pathLineParser :: String -> (Maybe Path)
pathLineParser s = Just $ (start, end) where
    start = s' !! 0
    end   = s' !! 1
    s'    = split '-' s

-- Reads the inputs from the designated input file
readInputs :: IO [Path]
readInputs = readInputFile inputFilePath pathLineParser

-- Counts the number of occurences of the given element in the specified list
count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

-- A test input
testInput :: [Path]
testInput = [
    ("start","A"),
    ("start","b"),
    ("A","c"),
    ("A","b"),
    ("b","d"),
    ("A","end"),
    ("b","end")
    ]

-- Another test input
testInput2 :: [Path]
testInput2 = [
    ("dc","end"),
    ("HN","start"),
    ("start","kj"),
    ("dc","start"),
    ("dc","HN"),
    ("LN","dc"),
    ("HN","end"),
    ("kj","sa"),
    ("kj","HN"),
    ("kj","dc")
    ]

-- Returns if the cave is the start or not
isStart :: String -> Bool
isStart = (==) "start"

-- Returns whether a cave is small
isSmallCave :: String -> Bool
isSmallCave = all isLower

-- Returns all valid paths to or from the specified cave
validPaths :: [Path] -> String -> [Path]
validPaths ps x = filter (\(s, e) -> s == x || e == x) ps

-- Returns all valid destinations from the specified cave
validDestinations :: [Path] -> String -> [String]
validDestinations ps x = map (\(s, e) -> if s == x then e else s) (validPaths ps x)

-- Flattens a full itenary tree into the individual complete paths
flattenIteneraries :: Itinerary String -> [[String]]
flattenIteneraries (Single a)     = [[a]]
flattenIteneraries (Branch a as)  = map (a:) (concatMap flattenIteneraries as)

-- Returns whether this is a valid transition (for part one)
validTransitionPart1 :: [Path] -> [String] -> String -> Bool
validTransitionPart1 ps cs d    | isStart d = False
                                | otherwise = (count d cs) < 1

-- Returns whether this is a valid transition (for part two)
validTransitionPart2 :: [Path] -> [String] -> String -> Bool
validTransitionPart2 ps cs d    | isStart d = False
                                | otherwise = case (any (\(_, n) -> n >= 2) (filter (\(x, _) -> isSmallCave x) (map (\xs -> (head xs, length xs)) ((group . sort) cs)))) of
                                    True  -> (count d cs) < 1
                                    False -> (count d cs) < 2

-- Computes all possible paths from the specified start to end
computeItineraries :: [Path] -> ([Path] -> [String] -> String -> Bool) -> String -> String -> [[String]]
computeItineraries ps f s e = filter (\i -> (last i) == e) (flattenIteneraries (computeItineraries' ps f e [] s)) where
    computeItineraries' ps f e cs s   | (e == s)  = Single s
                                    | otherwise = if (length ds'' > 0) then (Branch s ds'') else (Single s) where
                                        ds''    = if (length ds' > 0) then map (computeItineraries' ps f e cs') ds' else []
                                        ds'     = filter (f ps cs') ds
                                        ds      = (validDestinations ps s)
                                        cs'     = if (isSmallCave s) then (s:cs) else cs

-- Draws the iteneraries to screen
drawItineraries ::  [[String]] -> IO ()
drawItineraries = mapM_ (putStrLn . show)

-- The actual solver for the day 12 puzzle (part 1)
solvePart1 :: [Path] -> Int
solvePart1 ps = length (computeItineraries ps validTransitionPart1 "start" "end")

-- The actual solver for the day 12 puzzle (part 2)
solvePart2 :: [Path] -> Int
solvePart2 ps = length (computeItineraries ps validTransitionPart2 "start" "end")

-- The day 12 puzzle
day12Solver :: IO [Int]
day12Solver = do
    input <- readInputs
    --let input = testInput
    return [solvePart1 input, solvePart2 input]