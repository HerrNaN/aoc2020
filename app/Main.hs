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
             ,(8, show . day08a, show . day08b)
             ,(9, show . day09a 25, show . day09b 25)
             ,(10,show . day10a, show . day10b)
             ,(11,show . day11a, show . day11b)
             ,(12,show . day12a, show . day12b)
             ,(13,show . day13a, show . day13b)
             ,(14,show . day14a, show . day14b)
             ,(15,show . day15a, show . day15b)
             ,(16,show . day16a, show . day16b)
             ,(17,show . day17a, show . day17b)
             ,(18,show . day18a, show . day18b)
             ,(19,show . day19a, show . day19b)
             ,(20,show . day20a, show . day20b)
             ,(21,show . day21a, show . day21b)
            --  ,(22,show . day22a, show . day22b)
            --  ,(23,show . day23a, show . day23b)
            --  ,(24,show . day24a, show . day24b)
            --  ,(25,show . day25a, show . day25b)
             ]

-- | Formats the solution outputs for a given day in a nice way.
printDay :: String -> Day -> IO ()
printDay prefix (day, solvePartA, solvePartB) = do
    input <- getInput day
    putStrLn $           "Day "        ++ show day
    putStrLn $ prefix ++ " ├ Part A: " ++ solvePartA input
    putStrLn $ prefix ++ " └ Part B: " ++ solvePartB input

tooSlow :: String -> String
tooSlow _ = "TOO SLOW"