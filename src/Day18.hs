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
import Advent.Types (Part(Part2, Part1))

data Token = TInt Int | TOPar Int | TCPar Int | TMul | TAdd
    deriving Show
data BinOp = Mul | Add
    deriving Show
data Exp = EOp BinOp Exp Exp | EPar Exp | EInt Int
    deriving Show

type Grammar = Parsec String () Exp

doOp :: BinOp -> (Int -> Int -> Int)
doOp Mul = (*)
doOp Add = (+)

toOp :: Char -> BinOp
toOp '*' = Mul
toOp '+' = Add

day18a :: String -> Int
day18a = solveA . dayInput llExpr

solveA :: [Exp] -> Int
solveA = sum . map eval

eval :: Exp -> Int
eval (EInt i) = i
eval (EPar e) = eval e
eval (EOp op e e') = doOp op (eval e) (eval e')

day18b :: String -> Int
day18b = solveB . dayInput withPresidance

solveB :: [Exp] -> Int
solveB = sum . map eval

dayInput :: Grammar -> String -> [Exp]
dayInput ex = unsafeParse (parseLines1 ex) . reverseExpr

reverseExpr :: String -> String
reverseExpr = init . unlines . map (map swapParen . reverse) . lines
          
swapParen :: Char -> Char
swapParen '(' = ')'
swapParen ')' = '('
swapParen  c  =  c

llExpr :: Grammar
llExpr = expr

withPresidance :: Grammar
withPresidance = expr0

expr :: Parsec String () Exp
expr = do
        P.try (expr' `binop` expr)
    <|> P.try (paren expr)
    <|> P.try eint

expr' :: Parsec String () Exp
expr' = do
        P.try eint
    <|> P.try (paren expr)
    <|> P.try (expr' `binop` expr)

expr0 :: Parsec String () Exp
expr0 = do 
        P.try (expr0 `mul` expr1)
    <|> P.try expr1

expr1 :: Parsec String () Exp
expr1 = P.try (expr1 `add` expr2)
    <|> P.try expr2

expr2 :: Parsec String () Exp
expr2 = P.try eint 
    <|> P.try (paren expr0)

eint :: Parsec String () Exp
eint = EInt <$> parseInt

paren :: Parsec String () Exp -> Parsec String () Exp
paren ex = P.char '(' *> ex <* P.char ')'

binop :: Parsec String () Exp -> Parsec String () Exp -> Parsec String () Exp
binop ex ex' = do
    e <- ex
    op <- P.space *> P.oneOf "+*" <* P.space
    EOp (toOp op) e <$> ex'

add :: Parsec String () Exp -> Parsec String () Exp -> Parsec String () Exp 
add ex ex' = do
    e <- ex'
    P.space >> P.char '+' >> P.space
    EOp Add e <$> ex

mul :: Parsec String () Exp -> Parsec String () Exp -> Parsec String () Exp
mul ex ex' = do
    e <- ex'
    P.space >> P.char '*' >> P.space
    EOp Mul e <$> ex