module Day17 (day17Solver) where

import Common
import Parser
import Data.Char
import Data.List
import Data.Maybe

-- A simple 2D point in space
type Point = (Int, Int)

-- The probe
data Probe = Probe { position :: !Point, velocity :: !Point } deriving Show

-- The target area
data TargetArea = TargetArea { xArea :: !Point, yArea :: !Point } deriving Show

-- Creates a new probe at starting location (0,0) and the specified initial velocity
mkProbe :: Point -> Probe
mkProbe v0 = Probe { position=(0, 0), velocity=v0 }

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day17_input.txt"

-- Parses a single target area
targetAreaParser :: Parser TargetArea
targetAreaParser = do
    parseString "target area:"
    parseSpaces
    parseString "x="
    xMin<- parseInt
    parseString ".."
    xMax <- parseInt
    parseChar ','
    parseSpaces
    parseString "y="
    yMin<- parseInt
    parseString ".."
    yMax <- parseInt
    return TargetArea { xArea=(xMin, xMax), yArea=(yMin, yMax) }

-- A single state line parser
targetAreaLineParser :: String -> (Maybe TargetArea)
targetAreaLineParser s = Just $ runParser targetAreaParser s

-- Reads the inputs from the designated input file
readInputs :: IO TargetArea
readInputs = do
    inputs <- readInputFile inputFilePath targetAreaLineParser
    return $ head inputs

-- A simple test input
testInput :: TargetArea
testInput = runParser targetAreaParser "target area: x=20..30, y=-10..-5"

-- Runs a single step of the probe
stepProbe :: Probe -> Probe
stepProbe pr = Probe { position=p1, velocity=v1 } where
    p1          = (x0 + vx0, y0 + vy0)
    v1          = (vx0 - signum vx0, vy0 - 1)
    (x0, y0)    = position pr
    (vx0, vy0)  = velocity pr

-- Returns whether the probe is inside the target area
isInsideTargetArea :: TargetArea -> Point -> Bool
isInsideTargetArea ta (x, y) = if (x >= tmx) && (x <= tMx) && (y >= tmy) && (y <= tMy) then True else False where
    (tmx, tMx)  = xArea ta
    (tmy, tMy)  = yArea ta

-- Returns whether the probe can potentially reach - assuming the probe started off above the target area
canReachTargetArea :: TargetArea -> Point -> Bool
canReachTargetArea ta (x, y) = if (y < tmy) || (x > tMx) then False else True where
    (_, tMx)    = xArea ta
    (tmy, _)    = yArea ta

-- Runs the simulation on the probe & either returns the full trajectory if the target area was reached, or nothing
simulateProbe :: TargetArea -> Probe -> [Point]
simulateProbe ta pr = simulateProbe' ta pr [position pr] where
    simulateProbe' ta pr ps | (canReachTargetArea ta (position pr) == False)    = []
                            | (isInsideTargetArea ta (position pr) == True)     = reverse ps
                            | otherwise                                         = simulateProbe' ta pr' (p':ps) where
                                p'  = position pr'
                                pr' = stepProbe pr

-- Draws the simulation trajectory
drawSimulationTrajectory :: TargetArea -> [Point] -> IO ()
drawSimulationTrajectory ta ps = mapM_ (drawRow ta ps) [maxY,maxY-1..minY] where
    minY         = minimum ((fst (yArea ta)) : (map snd ps))
    maxY         = maximum ((snd (yArea ta)) : (map snd ps))
    drawRow ta ps y = putStrLn $ map (drawCell ta ps y) [minX..maxX] where
        minX     = minimum ((fst (xArea ta)) : (map fst ps))
        maxX     = maximum ((snd (xArea ta)) : (map fst ps))
        drawCell ta ps y x = case (findIndex (==(x, y)) ps) of 
            Just 0  -> 'S'
            Just _  -> '#'
            Nothing -> if isInsideTargetArea ta (x, y) then 'T' else '.'

-- The actual solver for the day 17 puzzle (part 1) - BRUTE FORCE
solvePart1 :: TargetArea -> Int
solvePart1 ta = (snd . last) simulations''' where
    simulations'''  = sortBy (\(_, psa) (_, psb) -> psa `compare` psb) simulations''
    simulations''   = map (\(p, ps) -> (p, (maximum (map snd ps)))) simulations'
    simulations'    = filter (not . null . snd) simulations
    simulations     = zip ps (map (simulateProbe ta) ps)
    ps              = [mkProbe (vx, vy) | vx <- [0..256], vy <- [0..256]]

-- The actual solver for the day 17 puzzle (part 2) - BRUTE FORCE
solvePart2 :: TargetArea -> Int
solvePart2 ta = length simulations' where
    simulations'    = filter (not . null . snd) simulations
    simulations     = zip ps (map (simulateProbe ta) ps)
    ps              = [mkProbe (vx, vy) | vx <- [0..1024], vy <- [-1024,-1023..1024]]

-- The day 17 puzzle
day17Solver :: IO [Int]
day17Solver = do
    input <- readInputs
    --let input = testInput
    return [solvePart1 input, solvePart2 input]