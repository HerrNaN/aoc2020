module Day01 where

import Parse

solveA :: String -> Int
solveA = solveA' . dayInput

solveA' :: [Int] -> Int
solveA' xs = x * y
    where (x,y) = head [(a, b) | a <- xs,
                                 b <- xs,
                                 a+b==2020]

solveB :: String -> Int
solveB = solveB' . dayInput

solveB' :: [Int] -> Int
solveB' xs = x * y * z
    where (x,y,z) = head [(a, b, c) | a <- xs,
                                      b <- xs,
                                      c <- xs,
                                      a+b+c==2020]

dayInput :: String -> [Int]
dayInput input = case parse (parseLines1 parseInt) input of
    Right db -> db
    Left e -> error $ show e