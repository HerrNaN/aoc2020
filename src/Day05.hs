{-# LANGUAGE FlexibleContexts #-}
module Day05 where

import Text.Parsec hiding (parse)
import Parse ( parse )
import Data.Functor (($>))
import Data.List (sort)

solveA :: String -> Int
solveA = solveA' . dayInput

solveA' :: [Pass] -> Int
solveA' = maximum . map passID

solveB :: String -> Int
solveB = solveB' . dayInput

solveB' :: [Pass] -> Int
solveB' ps = missing . sort $ map passID ps

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
dayInput input = case parse passes input of
    Right ps -> ps
    Left  e  -> error $ show e

passes :: Parsec String () [Pass]
passes = sepEndBy1 pass newline

pass :: Parsec String () Pass
pass = (,) <$> rows <*> cols

rows :: Parsec String () [Side]
rows = count 7 $ (char 'F' $> Low) <|> (char 'B' $> High)

cols :: Parsec String () [Side]
cols = count 3 $ (char 'L' $> Low) <|> (char 'R' $> High)