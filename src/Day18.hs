{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Day18 where

import Prelude hiding (lex)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)
import Data.Functor
import Parse
import Common
import Text.Parsec ((<|>), (<?>), Parsec)
import qualified Text.Parsec as P
import Data.Char (digitToInt)

data Token = TInt Int | TOPar Int | TCPar Int | TMul | TAdd
    deriving Show
data BinOp = Mul | Add
    deriving Show
data Exp = EOp BinOp Exp Exp | EPar Exp | EInt Int
    deriving Show

doOp :: BinOp -> (Int -> Int -> Int)
doOp Mul = (*)
doOp Add = (+)

toOp :: Char -> BinOp
toOp '*' = Mul
toOp '+' = Add

day18a :: String -> Int
day18a = solveA . dayInput "A"

solveA :: [Exp] -> Int
solveA = sum . map eval

eval :: Exp -> Int
eval (EInt i) = i
eval (EPar e) = eval e
eval (EOp op e e') = doOp op (eval e) (eval e')

day18b :: String -> Int
day18b = solveB . dayInput "B"

solveB :: [Exp] -> Int
solveB = sum . map eval

dayInput :: String -> String -> [Exp]
dayInput p input = case parse (parseLines1 expr') input' of
                    Right e -> e
                    Left  e -> error $ show e
    where input' = init $ unlines $ map (map swapParen . reverse) (lines input)
          expr'  | p == "A" = expr
                 | p == "B" = expr0
          

swapParen :: Char -> Char
swapParen '(' = ')'
swapParen ')' = '('
swapParen c   = c

expr :: Parsec String () Exp
expr = do
        P.try binop 
    <|> P.try paren
    <|> int

expr0 :: Parsec String () Exp
expr0 = do 
        P.try mul0
    <|> expr1

expr1 :: Parsec String () Exp
expr1 = P.try add1 <|> P.try expr2

expr2 :: Parsec String () Exp
expr2 = P.try int <|> paren0

int :: Parsec String () Exp
int = EInt <$> parseInt

paren0 :: Parsec String () Exp
paren0 = do
    P.char '('
    e <- expr0
    P.char ')'
    return e

paren :: Parsec String () Exp
paren = do
    P.char '('
    e <- expr
    P.char ')'
    return e

binop :: Parsec String () Exp
binop = do
    e <- P.try int <|> P.try paren <|> binop 
    op <- P.space *> P.oneOf "+*" <* P.space
    EOp (toOp op) e <$> expr

add1 :: Parsec String () Exp
add1 = do
    e <- expr2
    P.space >> P.char '+' >> P.space
    EOp Add e <$> expr1

mul0 :: Parsec String () Exp
mul0 = do
    e <- expr1
    P.space >> P.char '*' >> P.space
    EOp Mul e <$> expr0
