{-# LANGUAGE TupleSections #-}
module Common (
    freq,
    firstRepeat,
    findStable
) where

import qualified Data.Map as Map
import           Data.Map.Strict (Map)
import qualified Data.Set as Set
import           Data.Set (Set)

freq :: Ord a => [a] -> Map a Int
freq = Map.fromListWith (+) . map (,1)

firstRepeat :: Ord a => [a] -> Maybe a
firstRepeat = firstRepeat' Set.empty

firstRepeat' :: Ord a => Set a -> [a] -> Maybe a
firstRepeat' _ [] = Nothing
firstRepeat' seen (x:xs)
    | Set.member x seen = Just x
    | otherwise = firstRepeat' (Set.insert x seen) xs

findStable :: Eq a => [a] -> Maybe a
findStable [] = Nothing
findStable as@(a:a':_) = if a == a' then Just a else findStable (tail as)