{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Day19 where

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Text.Parsec ((<?>), Parsec)
import Data.Map (Map)
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Functor
import Control.Applicative
import Parse
import Common
import Data.List.Split (splitOn)
import Data.Either (isRight)
import Data.Functor.Identity (Identity(Identity))
import GHC.Conc (yield)
import qualified Text.Regex.PCRE as R 

type RuleMap = IntMap Rule
type Rule = Either [[Int]] Char

day19a :: String -> Int
day19a = solveA . dayInput

solveA :: (RuleMap, [String]) -> Int
solveA (rm,ms) = countTrue (`matchR` p) ms
    where p = toRegex 0 rm

matchR :: String -> String -> Bool
matchR s r = s R.=~ ("^" ++ r ++ "$")

toRegex :: Int -> RuleMap -> String
toRegex n rm = case rm IM.! n of
    Right c       -> [c]
    Left  [is]    -> "(" ++ concatMap (`toRegex` rm) is ++ ")"
    Left  [is,js] -> "(" ++ concatMap (`toRegex` rm) is ++ "|" ++ concatMap (`toRegex` rm) js ++ ")"

toRegexB :: Int -> RuleMap -> String
toRegexB 0 rm = "(" ++ toRegexB 8 rm ++ toRegexB 11 rm ++ ")"
toRegexB 8 rm = "(" ++ toRegex 42 rm ++ "+)"
toRegexB 11 rm = "(?'x'" ++ toRegex 42 rm ++ "(?P>x)?" ++ toRegex 31 rm ++ ")"

day19b :: String -> Int
day19b = solveB . dayInput

solveB :: (RuleMap, [String]) -> Int
solveB (rm,ms) = length $ filter (`matchR` p) ms
    where p = toRegexB 0 rm

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