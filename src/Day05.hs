{-# LANGUAGE FlexibleContexts #-}
module Day05 where

import Text.Parsec
import Parse (unsafeParse)
import Data.Functor (($>))
import Data.List (sort)

day05a :: String -> Int
day05a = solveA . dayInput

solveA :: [Pass] -> Int
solveA = maximum . map passID

day05b :: String -> Int
day05b = solveB . dayInput

solveB :: [Pass] -> Int
solveB ps = missing . sort $ map passID ps

missing :: [Int] -> Int
missing []  = -1
missing [_] = -1
missing (x:y:xs) | y - x /= 1 = x+1
                 | otherwise  = missing (y:xs)

passID :: Pass -> Int
passID (rs, cs) = row * 8 + col
    where (row, _) = foldl keep (0,127) rs 
          (col, _) = foldl keep (0,7)   cs


data Side = High | Low deriving (Show,Eq)
type Pass = ([Side], [Side])

keep ::  (Int, Int) -> Side -> (Int, Int)
keep (n,m) s | s == High = (n+d, m  )
             | s == Low  = (n  , m-d)
    where d = ceiling $ fromIntegral (m-n) / 2


dayInput :: String -> [Pass]
dayInput = unsafeParse passes

passes :: Parsec String () [Pass]
passes = sepEndBy1 pass newline

pass :: Parsec String () Pass
pass = (,) <$> rows <*> cols

rows :: Parsec String () [Side]
rows = count 7 $ (char 'F' $> Low) <|> (char 'B' $> High)

cols :: Parsec String () [Side]
cols = count 3 $ (char 'L' $> Low) <|> (char 'R' $> High)