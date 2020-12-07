{-# LANGUAGE RecordWildCards #-}
module Main where

import Days
import Input

type DayNumber = Integer
type Solution = String -> String
type Day = (DayNumber, Solution, Solution)
type Year = (Integer, [Day])

main :: IO ()
main = mapM_ printYear years

years :: [Year]
years = [(2020, days2020)]

printYear :: Year -> IO ()
printYear (year, days) = do
    putStrLn $ "Year " ++ show year
    let dropLast xs = take (length xs - 1) xs
    mapM_ (\d -> putStr " ├ " >> printDay " │ " d) $ dropLast days
    putStr " └ "
    printDay "   " $ last days

{-|
  The solutions, ready to run with the input. The format 
  is as follows:
  (DayNumber, solution A, solution B)
-}
days2020 :: [Day]
days2020 = [  (1, show . day01a, show . day01b)
             ,(2, show . day02a, show . day02b)
             ,(3, show . day03a, show . day03b)
             ,(4, show . day04a, show . day04b)
             ,(5, show . day05a, show . day05b)
             ,(6, show . day06a, show . day06b)
             ,(7, show . day07a, show . day07b)
             ]

-- | Formats the solution outputs for a given day in a nice way.
printDay :: String -> Day -> IO ()
printDay prefix (day, solvePartA, solvePartB) = do
    input <- getInput day
    putStrLn $           "Day "        ++ show day
    putStrLn $ prefix ++ " ├ Part A: " ++ solvePartA input
    putStrLn $ prefix ++ " └ Part B: " ++ solvePartB input
