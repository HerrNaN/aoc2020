module Day01 where

import Input

solveA :: String -> Int
solveA = solveA' . inputAsInts

solveA' :: [Int] -> Int
solveA' xs = x * y
    where (x,y) = head [(a, b)| a <- xs, b <- xs, a+b==2020]

solveB :: String -> Int
solveB = solveB' . inputAsInts

solveB' :: [Int] -> Int
solveB' xs = x * y * z
    where (x,y, z) = head [(a, b, c)| a <- xs, b <- xs, c <- xs, a+b+c==2020]