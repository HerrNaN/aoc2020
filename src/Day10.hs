{-# LANGUAGE TupleSections #-}
module Day10 where

import qualified Text.Parsec as P
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Parse
import Data.List (group, sort)
import Common

day10a :: String -> Int
day10a = solveA . dayInput

solveA :: [Int] -> Int
solveA is = Map.findWithDefault 0 1 m * (Map.findWithDefault 0 3 m + 1)
    where m = freq (diffs (sort (0:is ++ [maximum is])))

day10b :: String -> Int
day10b = solveB . dayInput

solveB :: [Int] -> Int
solveB is = go (0:is ++ [maximum is])
    where go = product . map (countWays . sum) . filter ((==1) . head) . group . diffs . sort

diffs :: Num c => [c] -> [c]
diffs xs@(_:ys)= zipWith (-) ys xs 

countWays :: Int -> Int
countWays 1 = 1
countWays 2 = 2
countWays 3 = 4
countWays n = countWays (n-1) + countWays (n-2) + countWays (n-3)

dayInput :: String -> [Int]
dayInput input = case parse (P.sepEndBy1 parseInt P.newline) input of
    Right is -> is
    Left  e  -> error (show e)