module Main where

import Common
import Day1

main :: IO ()
main = do
    putStrLn "== Advent of Code 2021 =="
    executeAndPrintResults "Day 1 (part 1 and 2)" day1Solver
