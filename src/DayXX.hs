{-# LANGUAGE FlexibleContexts #-}
module DayXX where

solveA :: String -> Int
solveA = solveA' . dayInput

solveA' :: a -> Int
solveA' = error "not implemented"

solveB :: String -> Int
solveB = solveB' . dayInput

solveB' :: a -> Int
solveB' = error "not implemented"

dayInput :: String -> a
dayInput = error "not implemented"