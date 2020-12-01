module Main where

import Lib

main :: IO ()
main = someFunc
-- main = mapM_ printSolution solutions

{-|
  The solutions, ready to run with the input. The format 
  is as follows:
  (Day, solution A, solution B)
-}
solutions :: [(Int, String -> String, String -> String)]
solutions = [
             -- (1, show . day01a, show . day01b)
             ]

-- | Formats the solution outputs for a given day in a nice way.
printSolution :: (Int, String -> String, String -> String) -> IO ()
printSolution (day, partA, partB) = do
                    input <- getInput day
                    putStrLn $ "Day " ++ show day
                    putStrLn $ "  Part A: " ++ partA input
                    putStrLn $ "  Part B: " ++ partB input

-- | Reads the input for a given day.
getInput :: Int -> IO String
getInput n | n < 10    = readFile $ "inputs/day0" ++ show n ++ ".txt"
           | otherwise = readFile $ "inputs/day" ++ show n ++ ".txt"