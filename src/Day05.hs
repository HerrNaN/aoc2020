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
solveB' ps = case  missing . sort $ map passID ps of
            Just x -> x
            Nothing -> -1

missing :: [Int] -> Maybe Int
missing []  = Nothing
missing [_] = Nothing
missing (x:y:xs) | y - x /= 1 = Just $ x+1
                 | otherwise  = missing (y:xs)
rows :: (Int, Int)
rows = (0,7)

cols :: (Int, Int)
cols = (0,127)

front :: (Int, Int) -> (Int, Int)
front range = keep range Low

back :: (Int, Int) -> (Int, Int)
back range = keep range High

left :: (Int, Int) -> (Int, Int)
left range = keep range Low

right :: (Int, Int) -> (Int, Int)
right range = keep range High

passID :: Pass -> Int
passID (cs, rs) = col * 8 + row
    where (row, _) = foldl keep rows rs 
          (col, _) = foldl keep cols cs


data Side = High | Low deriving (Show,Eq)
type Pass = ([Side], [Side])

keep ::  (Int, Int) -> Side -> (Int, Int)
keep (n,m) s | s == High = (n+d, m  )
             | s == Low  = (n  , m-d)
    where d = ceiling $ fromIntegral (m-n) / 2


dayInput :: String -> [Pass]
dayInput input = case parse parsePasses input of
    Right ps -> ps
    Left e -> error $ show e

parsePasses :: Parsec String () [Pass]
parsePasses = sepEndBy1 parsePass newline

parsePass :: Parsec String () Pass
parsePass = (,) <$> parseCols <*> parseRows

parseCols :: Parsec String () [Side]
parseCols = count 7 parseCol

parseRows :: Parsec String () [Side]
parseRows = count 3 parseRow

parseCol :: Parsec String () Side
parseCol = (char 'F' $> Low) <|> (char 'B' $> High)

parseRow :: Parsec String () Side
parseRow = (char 'L' $> Low) <|> (char 'R' $> High)