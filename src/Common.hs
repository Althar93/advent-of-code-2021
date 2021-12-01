module Common (
    LineParser (..), 
    readLineParser,
    readInputFile,
    executeAndPrintResults
    ) where

import System.IO
import Data.Maybe

-- A basic line parser
type LineParser a = String -> (Maybe a)

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
    result <- solver
    putStrLn $ title ++ " : " ++ show result