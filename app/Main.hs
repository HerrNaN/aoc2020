module Main where

import qualified Day01 as D01 
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06
import Input

type DayNumber = Int
type Solution = String -> String
type Day = (DayNumber, Solution, Solution)
type Year = (Int, [Day])

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
days2020 = [  (1, show . D01.solveA, show . D01.solveB)
             ,(2, show . D02.solveA, show . D02.solveB)
             ,(3, show . D03.solveA, show . D03.solveB)
             ,(4, show . D04.solveA, show . D04.solveB)
             ,(5, show . D05.solveA, show . D05.solveB)
             ,(6, show . D06.solveA, show . D06.solveB)
             ]

-- | Formats the solution outputs for a given day in a nice way.
printDay :: String -> Day -> IO ()
printDay prefix (day, solvePartA, solvePartB) = do
    input <- getInput day
    putStrLn $           "Day "        ++ show day
    putStrLn $ prefix ++ " ├ Part A: " ++ solvePartA input
    putStrLn $ prefix ++ " └ Part B: " ++ solvePartB input
