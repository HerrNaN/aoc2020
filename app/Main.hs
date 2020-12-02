module Main where

import Day01
import Input


main :: IO ()
main = mapM_ printSolution solutions

{-|
  The solutions, ready to run with the input. The format 
  is as follows:
  (DayNumber, solution A, solution B)
-}
solutions :: [(Int, String -> String, String -> String)]
solutions = [
              (1, show . solveA, show . solveB)
             -- (1, show . day01a, show . day01b)
             ]

-- | Formats the solution outputs for a given day in a nice way.
printSolution :: (Int, String -> String, String -> String) -> IO ()
printSolution (day, partA, partB) = do
                    input <- getInput day
                    putStrLn $ "Day " ++ show day
                    putStrLn $ "  Part A: " ++ partA input
                    putStrLn $ "  Part B: " ++ partB input
