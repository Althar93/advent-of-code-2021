module Common (
    LineParser (..), 
    split,
    readLineParser,
    readInputFile,
    executeAndPrintResults
    ) where

import System.IO
import Data.Time
import Data.Maybe
import Parser

-- A basic line parser
type LineParser a = String -> (Maybe a)

-- Splits a string based on the specified character
split :: Char -> String -> [String]
split c xs = case dropWhile (== c) xs of
                ""  -> []
                xs' -> w : split c xs'' where
                        (w, xs'') = break (== c) xs'

-- A simple line parser for types that implement read
readLineParser :: (Read a) => String -> (Maybe a)
readLineParser = Just . read

-- Reads a single input file and parses every single line
readInputFile :: FilePath -> LineParser a -> IO [a]
readInputFile fp lp = do
    contents <- readFile fp
    return $ mapMaybe lp (lines contents)

-- Convenience function for executing & printing the result of a puzzle
executeAndPrintResults :: (Show a) => String -> IO a -> IO ()
executeAndPrintResults title solver = do
    startTime <- getCurrentTime
    result <- solver
    putStrLn $ title ++ " : " ++ show result
    endTime <- getCurrentTime
    putStrLn $ "Took " ++ (show (diffUTCTime endTime startTime)) ++ " to execute."