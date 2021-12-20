module Day18 (day18Solver) where

import Common
import Parser
import Debug.Trace
import Data.List
import Data.Either
import Data.Maybe

-- A definition of a snail fish
data SnailFish = Pair SnailFish SnailFish | Number Int

-- A flattened snail fish consisting of a value and depth
type FlattenedSnailFish = [(Int, Int)]

--Flattens a fish to make explosions & splits easier to apply
flattenFish :: SnailFish -> FlattenedSnailFish
flattenFish s = flattenFish' s 0 where
    flattenFish' (Number n)   d = [(n, d - 1)]
    flattenFish' (Pair sa sb) d = (flattenFish' sa (d + 1)) ++ (flattenFish' sb (d + 1)) 

-- Unflattens a fish
unflattenFish :: FlattenedSnailFish -> SnailFish
unflattenFish f0 = case unflattenFish' 0 f0 of
    (sn, []) -> sn
    (sn, f') -> (Pair sn (unflattenFish f'))
    where
    unflattenFish' d ((x, xd):xs)
      | d == xd     = ((Number x),   xs)
      | otherwise   = ((Pair sa sb), xs'')
      where
        (sa, xs')   = unflattenFish' (d + 1) ((x, xd):xs)
        (sb, xs'')  = unflattenFish' (d + 1) xs'

-- Splits a flattened fish
splitFish :: FlattenedSnailFish -> Either FlattenedSnailFish FlattenedSnailFish
splitFish f0 = splitFish' f0 f0 where
    splitFish' f0 []                        = Left f0 
    splitFish' f0 ((xn, xd):xs) | xn >= 10  = Right $ ((xl, xd + 1):(xr, xd + 1):xs)
                                | otherwise = case splitFish' f0 xs of
                                        Right xs' -> Right $ ((xn, xd):xs')
                                        left      -> left
                                    where
                                        xl = floor   ((fromIntegral xn) / 2)
                                        xr = ceiling ((fromIntegral xn) / 2)

-- Injects a fish into the flattened fish
injectFish :: Int -> FlattenedSnailFish -> FlattenedSnailFish
injectFish _ []            = []
injectFish xr ((x, d):xs) = ((xr + x, d):xs)

--Explodes a flattened fish
explodeFish :: FlattenedSnailFish -> Either FlattenedSnailFish FlattenedSnailFish
explodeFish f0 = explodeFish' f0 f0 where
    explodeFish' f0 []                              = Left f0
    explodeFish' f0 ((xl, 4):(xr, 4):xs)            = Right $ (0, 3):(injectFish xr xs)
    explodeFish' f0 ((x0, d0):(xl, 4):(xr, 4):xs)   = Right $ (x0 + xl, d0):(0, 3):(injectFish xr xs)
    explodeFish' f0 (x:xs)                          = case explodeFish' f0 xs of 
                                                        Right xs'   -> Right $ (x:xs')
                                                        left        -> left

-- Reduces a snail fish once and returns either the original fish or a new reduced one
reduceFishOnce :: FlattenedSnailFish -> Either FlattenedSnailFish FlattenedSnailFish
reduceFishOnce f = case explodeFish f of 
    Left f -> splitFish f
    right  -> right

-- Reduces the given snail fish repeatedly until it can no longer be reduced
reduceFish :: FlattenedSnailFish -> FlattenedSnailFish
reduceFish s = case reduceFishOnce s of 
        Left  s  -> s
        Right s' -> reduceFish s'

addFish' :: FlattenedSnailFish -> FlattenedSnailFish -> FlattenedSnailFish
addFish' a b = map (\(x, d) -> (x, d + 1)) (a ++ b) 

-- Adds two snail fish together
addFish :: SnailFish -> SnailFish -> SnailFish
addFish sa sb = Pair sa sb

-- Make snail fish an instance of show so we can more easily show what is going on
instance Show SnailFish where
    -- SnailFish -> String
    show (Pair sa sb) = "{" ++ show sa ++ "," ++ show sb ++ "}"
    show (Number n)   = show n

-- Parses a single snail fish
parseSnailFish :: Parser SnailFish
parseSnailFish = do
    parseChar '['
    leftValue <- pEither parseInt parseSnailFish
    parseChar ','
    rightValue <- pEither parseInt parseSnailFish
    parseChar ']'
    return $ Pair (eitherSnailFishValue leftValue) (eitherSnailFishValue rightValue) where
        eitherSnailFishValue = either (\a -> Number a) (\x -> x)

-- Makes a snail fish from a string
mkSnailFish :: String -> SnailFish
mkSnailFish = runParser parseSnailFish

-- The input file path
inputFilePath :: FilePath
inputFilePath = "res/day18_input.txt"

-- A single snail fish line parser
snailFishLineParser :: String -> (Maybe SnailFish)
snailFishLineParser s = Just $ mkSnailFish s

-- Reads the inputs from the designated input file
readInputs :: IO [SnailFish]
readInputs = readInputFile inputFilePath snailFishLineParser

-- A test input
testInput :: [SnailFish]
testInput = [
    mkSnailFish "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
    mkSnailFish "[[[5,[2,8]],4],[5,[[9,9],0]]]",
    mkSnailFish "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
    mkSnailFish "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
    mkSnailFish "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
    mkSnailFish "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
    mkSnailFish "[[[[5,4],[7,7]],8],[[8,3],8]]",
    mkSnailFish "[[9,3],[[9,9],[6,[4,9]]]]",
    mkSnailFish "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
    mkSnailFish "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
    ]

testInput2 :: [SnailFish]
testInput2 = [
    mkSnailFish "[1,1]",
    mkSnailFish "[2,2]",
    mkSnailFish "[3,3]",
    mkSnailFish "[4,4]"
    ]

testInput3 :: [SnailFish]
testInput3 = [
    mkSnailFish "[1,1]",
    mkSnailFish "[2,2]",
    mkSnailFish "[3,3]",
    mkSnailFish "[4,4]",
    mkSnailFish "[5,5]"
    ]

testInput4 :: [SnailFish]
testInput4 = [
    mkSnailFish "[1,1]",
    mkSnailFish "[2,2]",
    mkSnailFish "[3,3]",
    mkSnailFish "[4,4]",
    mkSnailFish "[5,5]",
    mkSnailFish "[6,6]"
    ]

testInput5 :: [SnailFish]
testInput5 = [
    mkSnailFish "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]",
    mkSnailFish "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]",
    mkSnailFish "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]",
    mkSnailFish "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]",
    mkSnailFish "[7,[5,[[3,8],[1,4]]]]",
    mkSnailFish "[[2,[2,2]],[8,[8,1]]]",
    mkSnailFish "[2,9]",
    mkSnailFish "[1,[[[9,3],9],[[9,0],[0,7]]]]",
    mkSnailFish "[[[5,[7,4]],7],1]",
    mkSnailFish "[[[[4,2],2],6],[8,7]]"
    ]

-- Returns the computed magnitude of the given snail fish
fishMagnitude :: SnailFish -> Int
fishMagnitude (Pair sa sb) = 3 * (fishMagnitude sa) + 2 * (fishMagnitude sb)
fishMagnitude (Number n)   = n

-- Sums multiple snail fish together (includes reduction)
sumFish :: [SnailFish] -> SnailFish
sumFish [] = error "Needs at least one fish"
sumFish ss = unflattenFish ff' where
    ff' = foldl1 (\a b -> reduceFish (addFish' a b)) ff
    ff  = map flattenFish ss

-- The actual solver for the day 18 puzzle (part 1)
solvePart1 :: [SnailFish] -> Int
solvePart1 [] = 0
solvePart1 ss = (fishMagnitude . sumFish) ss

-- The actual solver for the day 18 puzzle (part 2)
solvePart2 :: [SnailFish] -> Int
solvePart2 ss = maximum fishMagnitudes where
    fishMagnitudes = map (fishMagnitude . sumFish) sss
    sss = [[ss !! x, ss !! y] | x <- [0..(length ss)-1], y <- [0..(length ss)-1], x /= y]

-- The day 18 puzzle
day18Solver :: IO [Int]
day18Solver = do
    input <- readInputs
    --let input = testInput
    return [solvePart1 input, solvePart2 input]