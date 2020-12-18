{-# LANGUAGE RecordWildCards #-}
module Day16 where

import qualified Text.Parsec as P
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec (Parsec)
import Data.Map (Map)
import Data.Set (Set)
import Data.Functor
import Control.Applicative
import Parse
import Common
import Debug.Trace
import Data.List.Split
import Data.List


type Range = (Int, Int)
data Rule  = R
    { rName :: String
    , rRanges :: (Range, Range)
    } deriving (Show)
type Ticket = [Int]
data Document = D
    { dRules  :: [Rule]
    , dYours  :: Ticket
    , dNearby :: [Ticket]
    } deriving (Show)

ticketInvalid :: [Rule] -> Ticket -> [Int]
ticketInvalid rs fs = map snd $ filter fst $ map (fieldInvalid rs) fs

fieldInvalid :: [Rule] -> Int -> (Bool, Int)
fieldInvalid rs f = (all (fieldInvalidForRule f) rs, f)

fieldInvalidForRule :: Int -> Rule -> Bool
fieldInvalidForRule f R{..} = not (inRange r f) && not (inRange r' f)
    where (r, r') = rRanges

inRange :: (Int, Int) -> Int -> Bool
inRange (min,max) i = min <= i && i <= max

day16a :: String -> Int
day16a = solveA . dayInput

solveA :: Document -> Int
solveA D{..} = sum $ concatMap (ticketInvalid dRules) dNearby



day16b :: String -> Int
day16b = solveB . dayInput

solveB :: Document -> Int
solveB d@D{..} = product $ map (dYours !!) idxs
    where frm = matchRulesWithField dRules $ validTickets d
          pm = M.filterWithKey (\k _ -> hasPrefix "departure" k) $ deduce frm
          idxs = M.elems pm

deduce :: Map String [Int] -> Map String Int
deduce m = deduce' m (M.size m) M.empty

deduce' :: Map String [Int] -> Int -> Map String Int -> Map String Int
deduce' _  0 im = im
deduce' pm n im = deduce' pm' (n-1) im'
    where (rule, index:_) = M.elemAt 0 $ M.filter ((==1) . length) pm
          im' = M.insert rule index im
          pm' = M.map (delete index) pm

validTickets :: Document -> [Ticket]
validTickets D{..} = filter (null . ticketInvalid dRules) dNearby

matchRulesWithField :: [Rule] -> [Ticket] -> Map String [Int]
matchRulesWithField rs ts = M.fromList $ map (matchRuleWithField ts) rs

matchRuleWithField :: [Ticket] -> Rule -> (String, [Int])
matchRuleWithField ts r@R{..} = (rName, map fst $ filter (fits r . snd) is)
    where is =  zip [0..] $ transpose ts

fits :: Rule -> [Int] -> Bool
fits r  = all (fitsInRule r)

fitsInRule :: Rule -> Int -> Bool
fitsInRule R{..} i = inRange r i || inRange r' i
    where (r,r') = rRanges 

hasPrefix :: String -> String -> Bool
hasPrefix p s
    | length p > length s = False
    | otherwise = all (==True) $ zipWith (==) p s

dayInput :: String -> Document
dayInput input = D rs' yt' nts'
    where [rs, yt, nts] = splitOn "\n\n" input
          rs'  = unsafeParse rules rs
          yt'  = unsafeParse yourTicket yt
          nts' = unsafeParse nearbyTickets nts

rules :: Parsec String () [Rule]
rules = P.sepBy1 rule P.newline

yourTicket :: Parsec String () [Int]
yourTicket = do
    P.string "your ticket:"
    P.newline
    ticket

nearbyTickets :: Parsec String () [Ticket]
nearbyTickets = do
    P.string "nearby tickets:"
    P.newline
    P.sepEndBy1 ticket P.newline

rule :: Parsec String () Rule
rule = do
    name <- P.manyTill (P.noneOf "\n") (P.char ':')
    P.space
    r <- range
    P.string " or "
    r' <- range
    return $ R name (r,r')

range :: Parsec String () (Int, Int)
range = do
    min <- parseInt
    P.char '-'
    max <- parseInt
    return (min,max)

ticket :: Parsec String () [Int]
ticket = P.sepBy1 (read <$> P.many1 P.digit) (P.char ',') 


traceShow' s = traceShow s s