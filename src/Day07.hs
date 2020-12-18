module Day07 where

import Text.Parsec
import Parse
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Functor (($>))

day07a :: String -> Int
day07a = solveA . dayInput

solveA :: [Rule] -> Int
solveA rs = S.size $ canContain (fromRulesA rs) ("shiny", "gold")

canContain :: M.Map Bag [Bag] -> Bag -> S.Set Bag
canContain = canContain' S.empty

canContain' :: S.Set Bag -> M.Map Bag [Bag] -> Bag -> S.Set Bag
canContain' s m b | member    = S.union s' $ S.fromList bs
                  | otherwise = s
            where member = M.member b m
                  bs = m M.! b
                  s' = S.unions $ map (canContain' s m) bs

fromRulesA :: [Rule] -> M.Map Bag [Bag]
fromRulesA = fromRulesA' M.empty

fromRulesA' :: M.Map Bag [Bag] -> [Rule] -> M.Map Bag [Bag]
fromRulesA' = foldr insertRuleA

insertRuleA :: Rule -> M.Map Bag [Bag] -> M.Map Bag [Bag]
insertRuleA (_,[]) m = m   
insertRuleA (b,(_,b'):bs') m = M.insertWith (++) b' [b] $ insertRuleA (b, bs') m

day07b :: String -> Int
day07b = solveB . dayInput

solveB :: [Rule] -> Int
solveB rs = countBags ("shiny", "gold") (fromRulesB rs) - 1

fromRulesB :: [Rule] -> M.Map Bag [(Int, Bag)]
fromRulesB = M.fromList

countBags :: Bag -> M.Map Bag [(Int, Bag)] -> Int
countBags b m | bs /= [] = 1 + sum (map (\(n,b') -> n * countBags b' m) bs)
              |otherwise = 1
    where bs = m M.! b

dayInput :: String -> [Rule]
dayInput = unsafeParse rules 

type Rule = (Bag, [(Int, Bag)])
type Bag = (String, String)

rules :: Parsec String () [Rule]
rules = sepEndBy1 rule newline

rule :: Parsec String () Rule
rule = do
    key <- bag
    string " contain "
    vals <- (try (string "no other bags") $> []) <|> sepBy1 nBag (string ", ")
    char '.'
    return (key, vals)

bag :: Parsec String () Bag
bag = do 
    mod <- manyTill letter space
    col <- manyTill letter space
    try (string "bags") <|> string "bag"
    return (mod, col)

nBag :: Parsec String () (Int, Bag)
nBag = do
    n <- parseInt
    space
    bag <- bag
    return (n,bag)