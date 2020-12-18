{-# LANGUAGE FlexibleContexts #-}
module Day06 where

import qualified Data.Set as S
import Text.Parsec
import Parse ( unsafeParse )
import Data.List.Split (splitOn)

type Group = S.Set Char

day06a :: String -> Int
day06a = solveA . dayInput (unsafeParse groupA)

solveA :: [Group] -> Int
solveA = sum . map S.size

day06b :: String -> Int
day06b = solveB . dayInput (unsafeParse groupB)

solveB :: [Group] -> Int
solveB = sum . map S.size

dayInput :: (String -> Group) -> String -> [Group]
dayInput p = map p . splitOn "\n\n"

groupA :: Parsec String () Group
groupA = do 
    ps <- sepEndBy1 person newline
    return $ S.unions ps

groupB :: Parsec String () Group
groupB = do
    (p:ps) <- sepEndBy1 person newline
    return $ foldl S.intersection p ps
    

person :: Parsec String () (S.Set Char)
person = S.fromList <$> many1 answer

answer :: Parsec String () Char
answer = oneOf "abcdefghijklmnopqrstuvwxyz"