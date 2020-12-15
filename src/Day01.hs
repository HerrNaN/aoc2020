{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day01 (
    day01a,
    day01b
) where

import Parse

day01a :: String -> Int
day01a = solveA . dayInput

solveA :: [Int] -> Int
solveA xs = x * y
    where (x,y) = head [(a, b) | a <- xs,
                                 b <- xs,
                                 a+b==2020]

day01b :: String -> Int
day01b = solveB . dayInput

solveB :: [Int] -> Int
solveB xs = x * y * z
    where (x,y,z) = head [(a, b, c) | a <- xs,
                                      b <- xs,
                                      c <- xs,
                                      a+b+c==2020]

dayInput :: String -> [Int]
dayInput input = case parse (parseLines1 parseInt) input of
    Right db -> db
    Left e -> error $ show e