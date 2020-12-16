module Day15 where

import Data.List.Split (splitOn)
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Vector.Unboxed.Mutable (MVector)
import Control.Monad.ST (runST, ST)
import Control.Monad (forM_)
import Control.Monad.State.Strict (execStateT, put, StateT(..),get)

day15a :: String -> Int
day15a = solveA . dayInput

solveA :: [Int] -> Int
solveA xs = runST $ execStateT (speakN 2020 xs) (last xs)

speakN :: Int -> [Int] -> StateT Int (ST s) ()
speakN n xs = do
    v <- MV.replicate (n + length xs) 0
    forM_ (zip [1..] (init xs)) (\(i, y) -> MV.write v y i)
    forM_ [length xs .. n - 1] (speak v)

speak :: MVector s Int -> Int -> StateT Int (ST s)  ()
speak v n = do
    lastSaid <- get
    pOcc <- MV.read v lastSaid
    MV.write v lastSaid n
    if pOcc == 0 then put 0
    else put (n-pOcc)

day15b :: String -> Int
day15b = solveB . dayInput

solveB :: [Int] -> Int
solveB xs = runST $ execStateT (speakN 30000000 xs) (last xs)

dayInput :: String -> [Int]
dayInput = map read . splitOn ","