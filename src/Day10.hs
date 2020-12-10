module Day10 where

import qualified Text.Parsec as P
import qualified Data.Map as Map
import Data.Map.Strict (Map)
import Parse
import Data.List (group, sort)

day10a :: String -> Int
day10a = solveA . dayInput

solveA :: [Int] -> Int
solveA is = Map.findWithDefault 0 1 m * (Map.findWithDefault 0 3 m + 1)
    where m = diffFreq $ sort is

diffFreq :: [Int] -> Map Int Int
diffFreq = snd . foldl f (0, Map.empty)
    where f (a, m) b = (b, Map.insertWith (+) (b-a) 1 m)

day10b :: String -> Int
day10b = solveB . dayInput

solveB :: [Int] -> Int
solveB = product . map (countWays . length) . filter ((==1) . head) . group . diffList . sort 

diffList :: [Int] -> [Int]
diffList = snd . foldl f (0, [])
    where f (i, xs) a = (a, (a-i):xs)

countWays :: Int -> Int
countWays 1 = 1
countWays 2 = 2
countWays 3 = 4
countWays n = countWays (n-1) + countWays (n-2) + countWays (n-3)

dayInput :: String -> [Int]
dayInput input = case parse (P.sepEndBy1 parseInt P.newline) input of
    Right is -> is
    Left  e  -> error (show e)