{-# LANGUAGE TupleSections #-}
module Day19 where

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Text.Parsec (Parsec)
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Functor
import Control.Applicative
import Parse
import Common
import Data.List.Split (splitOn)

type RuleMap = IntMap Rule
type Rule = Either [[Int]] Char

day19a :: String -> Int
day19a = solveA . dayInput

solveA :: (RuleMap, [String]) -> Int
solveA (rm,ms) = length $ filter (match 0 rm) ms

match :: Int -> IntMap Rule -> String -> Bool
match n rm s = s `S.member` vs
    where vs = S.fromList $ resolveRule n rm

resolveRule :: Int -> RuleMap -> [String]
resolveRule n rm = case rm IM.! n of
    Right c      -> [[c]]
    Left [is]    -> foldl combined [] $ map (`resolveRule` rm) is
    Left [is,js] -> foldl combined [] (concatRules rm is)
                 ++ foldl combined [] (concatRules rm js)

combined :: [String] -> [String] -> [String]
combined []  []  = []
combined []  bs  = bs
combined as  []  = as
combined [a] bs  = map (a ++) bs
combined as  [b] = map (++ b) as
combined as bs   = concatMap (\a -> combined [a] bs) as


concatRules :: RuleMap -> [Int] -> [[String]]
concatRules rm = map (`resolveRule` rm)

day19b :: String -> Int
day19b = solveB . dayInput

solveB :: (RuleMap, [String]) -> Int
solveB = error "not implemented"

dayInput :: String -> (RuleMap, [String])
dayInput input = (rs', lines ms)
    where [rs, ms] = splitOn "\n\n" input
          rs' = IM.fromList $ unsafeParse (parseLines1 rule) rs

rule :: Parsec String () (Int, Rule)
rule = do
    n <- parseInt
    P.string ": "
    (\c -> (n,Right c)) <$> (P.char '\"' *> P.oneOf "ab" <* P.char '\"') <|>
        (\rss -> (n, Left rss)) <$> P.sepBy1 subRule (P.try $ P.string " | ")

subRule :: Parsec String () [Int]
subRule = P.sepBy1 parseInt (P.notFollowedBy (P.string " | ") >> P.char ' ')



