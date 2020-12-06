{-# LANGUAGE FlexibleContexts #-}
module Day02 where

import qualified Text.Parsec as Parsec
import Parse
import Text.Parsec ((<?>))

type Policy = (Int, Int, Char)
type Password = String
type Entry = (Policy, Password)
type Database = [Entry]

day02a :: String -> Int
day02a = solveA . dayInput

solveA :: Database -> Int
solveA = length . filter entryValidA

day02b :: String -> Int
day02b = solveB . dayInput

solveB :: Database -> Int
solveB = length . filter entryValidB

entryValidA :: Entry -> Bool
entryValidA ((min, max, char), pass) = count >= min && count <= max
    where count = length $ filter (==char) pass

entryValidB :: Entry -> Bool
entryValidB ((pos1, pos2, char), pass) = atPos1 `xor` atPos2
    where atPos1 = pass!!(pos1-1) == char
          atPos2 = pass!!(pos2-1) == char

xor :: Bool -> Bool -> Bool
True  `xor` False = True
False `xor` True  = True
_     `xor` _     = False

dayInput :: String -> Database
dayInput input = case parse parseDatabase input of
    Right db -> db
    Left e -> error $ show e

parseDatabase :: Parsec.Parsec String () Database
parseDatabase = parseLines1 parseEntry

parseEntry :: Parsec.Parsec String () Entry
parseEntry = do
    pol <- parsePolicy -- <?> error "expected Policy"
    Parsec.string ": "
    pass <- parsePassword <?> error "expected Password"
    return (pol, pass)

parsePolicy :: Parsec.Parsec String () Policy
parsePolicy = do
    min <- parseInt -- <?> error "expected Int"
    Parsec.char '-'
    max <- parseInt <?> error "cexpected Int"
    Parsec.space
    c <- Parsec.letter
    return (min, max, c)

parsePassword :: Parsec.Parsec String () Password
parsePassword = Parsec.many1 Parsec.letter

