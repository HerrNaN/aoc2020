{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Day03 where

import qualified Text.Parsec as Parsec
import Parse

data Point = Tree | None
    deriving Eq
type Forest = [[Point]]

day03a :: String -> Int
day03a = solveA . dayInput

solveA :: Forest -> Int
solveA pss = treesOnSlope pss (3,1)

day03b :: String -> Int
day03b = solveB . dayInput

solveB :: Forest -> Int
solveB pss = product trees
    where trees  = map (treesOnSlope pss) slopes
          slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]

atRow :: Int -> [Point] -> Bool
atRow n ps = (ps !! (n `mod` length ps)) == Tree

treesOnSlope :: Forest -> (Int, Int) -> Int
treesOnSlope pss (col, row) = sum $ map (fromEnum . inForest pss) slope
    where slope = points col row $ length pss

inForest :: Forest -> (Int, Int) -> Bool
inForest pss (col, row) = atRow col $ pss !! row

points :: Int -> Int -> Int -> [(Int, Int)]
points col row rows = take (rows `quot` row) ps
    where ps = zip (map (col*) [0..]) $ map (row*) [0..]

dayInput :: String -> Forest
dayInput input = case parse parseForest input of
    Right db -> db
    Left e -> error $ show e

parseForest :: Parsec.Parsec String () Forest
parseForest = parseLines1 parseEntry

parseEntry :: Parsec.Parsec String () [Point]
parseEntry = Parsec.many1 parsePoint

parsePoint :: Parsec.Parsec String () Point
parsePoint = Parsec.oneOf ".#" >>= \case
        '.' -> return None
        '#' -> return Tree

